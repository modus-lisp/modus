# The I/O core: USB and networking that survive the Pi Zero 2 W

## Why this exists

The Zero 2 W has one USB controller, the Synopsys DesignWare OTG (DWC2), and
every device the desk uses sits behind it: the RTL8153 network adapter, the
keyboard and mouse (full and low speed, behind a hub, so every transfer to them
is a SPLIT transaction).  The DWC2 does not schedule transfers: software
programs each host channel, watches it halt, handles NAKs and re-arms it, and
sequences start-split/complete-split inside the 125 µs microframes.  Linux's
Pi driver meets that timing by servicing the controller from the FIQ about
8000 times a second.

Until now modus drove it by POLLING from whichever thread happened to want a
transfer, with no interrupts, no single owner and no recovery.  Measured
consequence (2026-10-31): a hub control transfer issued while the NIC's
bulk-IN was armed made the controller disable the root port, and with the
port went the network, the keyboard and the mouse, permanently.  The fix that
shipped (pause the NIC's receive around the HID hotplug scan) patches one
interaction; the design below removes the class.

## The design

1. **One core owns the controller.**  Core 1 runs the I/O loop and nothing
   else is pinned there.  Every DWC2 register access happens on it, so there
   are no cross-core races on channel state, and policies like "no split
   transaction while a high-speed bulk-IN is NAK-spinning" are enforced by the
   one scheduler instead of by hooks between drivers.
2. **It never conses.**  Its state, its request and completion rings and its
   DMA buffers are fixed RAM outside the heap.  So it never collects, never
   holds a heap pointer, and is permanently GC-SAFE: a stop-the-world
   collection of region 0 does not wait for it.
3. **It is interrupt-woken, without an ISR.**  PSTATE.I stays masked on the
   I/O core; an interrupt is never taken, it only ends a WFI.  The loop then
   reads GINTSTS/HAINT/HCINTn and services channels in ordinary compiled code,
   with no reentrancy and no saved context.  Inside timing-critical windows
   (split sequences) it simply spins: the core has nothing else to do.
4. **Other cores talk to it through rings**: transfer requests in, completions
   out; received frames into a frame ring the network stack drains; HID
   reports into an event ring.  The hub's status-change endpoint becomes a
   periodic interrupt transfer instead of polled control transfers.
5. **Recovery.**  The loop watches HPRT0 and per-channel error streaks; on a
   port disable or a wedged channel it resets the port, re-enumerates the
   remembered topology, rebinds the class drivers and posts link-down/up
   events.  A fault costs dropped packets, not the board.
6. **The network stack always runs**: a thread that drains the frame ring
   whatever owns the main thread (the desk does).  It conses, so it is not on
   the I/O core.
7. **Evidence**: counters in the control block, readable from any core; the
   message log (`lib/bare-log.lisp`, `(modus:klog-dump)`); a check that the
   loop allocates nothing; and the 60-second netboot soak as a standing board
   test.

## Stage 1: interrupt discipline (net/usb-io-core.lisp)

The routing, three hops, all of which must be open:

| hop | register | what |
|---|---|---|
| DWC2 | GAHBCFG bit 0, GINTMSK | which events raise the line |
| BCM2835 | Enable IRQs 1 (0x3F00B210) bit 9 | USB into the GPU interrupt |
| BCM2836 | GPU_ROUTE (0x4000000C) bits 1:0 | the GPU interrupt to ONE core |

and on the core, HCR_EL2.IMO: this image runs at EL2, where an IRQ with IMO
clear targets EL1 (a lower level) and is never taken here.  Setting IMO makes
it target EL2, where PSTATE.I keeps it from being taken and WFI wakes on it.
The BCM2836 local controller is outside the boot page tables;
`io-map-local` maps it as bare-perf does.

**This is a regression being repaired, not new ground.**  The mini-Lisp board
images (tag `mini-lisp-final`, `net/arch-raspi3b.lisp` `dwc2-enable-host-irq`)
woke WFI on DWC2 channel interrupts with IRQs masked and armed a 1 ms virtual
timer for the idle delay.  The CL port stubbed both to NIL on the stated
grounds that the image "programs no interrupt controller ... so WFI would
never be woken", which is false: no exception is ever taken.

Measured under QEMU raspi3b (`io-setup CORE GINTMSK`, then
`io-wake-probe CORE MS IMO`, then `io-report`), 300 ms with GINTMSK = SOF:

| core | wakes | USB | timer | other |
|---|---|---|---|---|
| 0 (main) | 284 | 1 | 283 | 0 |
| 1 (an executor) | 287 | 1 | 286 | 0 |

One USB wake per run because the probe masks the source after it (it services
nothing); the rest are the 1 ms virtual-timer tick, so WFI really sleeps.
QEMU wakes WFI on any pending interrupt, so it cannot answer the IMO question;
the Zero can.  Note the timer re-arm (`timer-rearm`, trap #x0321) writes 62500
ticks: 1 ms at QEMU's 62.5 MHz, 3.3 ms at the Zero's 19.2 MHz.

**On the Zero 2 W (silicon), 300 ms, GINTMSK = SOF, HCR_EL2 as the firmware left
it = `0x2` on cores 1-3 (`0x22` on core 0): IMO is REQUIRED.**

| core | HCR_EL2.IMO | wakes | USB | timer | other |
|---|---|---|---|---|---|
| 3 | set by the probe | 184 | 1 | 92 | 91 |
| 2 | clear | **none: the WFI never ended** | | | |

At EL2 an IRQ that targets EL1 does not even wake WFI on the BCM2837, so every
I/O core must set IMO before it sleeps (QEMU wakes regardless and cannot show
this).  92 timer wakes = 300 ms / 3.26 ms, the 62500-tick re-arm at 19.2 MHz.
The 91 "other" wakes follow the timer ones one-for-one: just after a re-arm the
line still reads pending once, then the source register is clear.  Harmless,
but the I/O loop must re-check its sources rather than trust a wake.

**Found on the way: executors were not pinned where asked.**  BT-START-EXECUTOR
did BT-PIN-NEXT CORE and then BT-SPAWN, whose own least-loaded choice overwrote
the pin: executors asked for cores 2 and 3 (started first) ran on 1 and 2.  The
desk's USB/screen/audio executors on 1/2/3 were right only because the desk
starts them in that order.  Fixed (the core goes to BT-SPAWN), and the probe now
refuses (returns -1, logs `IOWK core actual -1`) on the wrong core instead of
sleeping forever.  The hung boot's own record, read after the reset with
`(modus:klog-dump :boot :previous)`, is what showed the probe had run on `c2`.

## Stage 2: the engine (USBE-MAIN, net/usb-io-core.lisp)

`(modus-int::usbe-start)` from the main thread starts a bare thread alone on core 1
that owns the DWC2 from then on; `usbe-stop` hands it back.  The NIC interface
(`usb-nic-*`), `net-rx-idle` and the fast-ACK path route to its rings while it runs.
`(modus-int::usbe-status)` / `(modus-int::usbe-profile)` / `(modus:image-configuration)`
read it.  It needs the RTL8153 bound (`:no-rtl8153` otherwise; CDC-ECM is not wired).

Measured on the Zero 2 W, the network running entirely through the engine (ping from
the builder, SSH evaluations over the same link):

| | first cut | `ceiling` fixed | + background in split waits |
|---|---|---|---|
| allocating passes | 950 of 2527 | 1 (a misplaced baseline) | **0** |
| ping RTT avg / max | 26-34 ms / 57 ms | 35.6 / 462 ms | **5.2** / 489 ms |
| ping loss (30-60 s) | 0 | 1 of 300 | 2 of 300 |
| RX / TX errors | 0 / 0 | 0 / 0 | 0 / 0 |
| root port | enabled throughout | enabled throughout | enabled throughout |

**THE ALLOCATION WAS `CEILING`'S `&REST` LIST.**  `(ceiling len mps)` conses one on
every two-argument call, and every transfer setup computed its packet count that way.
`dwc2-packets` is the inline `(truncate (+ len mps -1) mps)`.  The per-service profile
found it: allocating calls per service went from non-zero to 0 everywhere.

**AN HID POLL IS ~19 ms OF MICROFRAME WAITS** (keyboard + mouse behind the hub, split
transactions, 95% of the engine's time).  Those waits now serve the receive and the send
once each (`usbe-background`, only during HID polls -- never during a hub scan, where
the receive is halted on purpose), which is the 35 -> 5 ms.  The next step is
asynchronous splits: start a split, return to the loop, continue it at the next
microframe from the SOF interrupt.

**OPEN:** one hotplug scan right after the start takes ~506 ms with the receive halted
(the worst ping and the lost ones); unexplained yet.
