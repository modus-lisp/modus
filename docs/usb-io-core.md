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
   message log (`lib/bare-log.lisp`, `(modus:kmsg-dump)`); a check that the
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
`(modus:kmsg-dump :boot :previous)`, is what showed the probe had run on `c2`.

## Stage 2: the engine (USBE-MAIN, net/usb-io-core.lisp)

`(modus-int::usbe-start)` from the main thread starts a bare thread alone on core 1
that owns the DWC2 from then on; `usbe-stop` hands it back.  The NIC interface
(`usb-nic-*`), `net-rx-idle` and the fast-ACK path route to its rings while it runs.
`(modus-int::usbe-status)` / `(modus-int::usbe-profile)` / `(modus:image-configuration)`
read it.  It drives whatever NIC is bound when it starts -- the RTL8153 in its vendor
mode, a CDC-ECM adapter, or none (the keyboard, mouse and hubs alone).

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

The long hotplug scan right after the start (~506 ms with the receive halted) is gone
with the tree's scan (below): no scan on the board takes more than 20 ms.

## Stage 3: the keyboard and mouse under the engine

The polls are a state machine (`usbe-hid-step`): program a split, return to the loop,
continue when the channel halts or the next SOF arrives.  It never worked on the board --
every CSPLIT was answered NYET until it gave up -- and the cause was not the state
machine:

**WITH THE NIC'S BULK-IN ARMED, EVERY PERIODIC SPLIT NYETS.**  Measured on the Zero,
deterministic: halt channel 1 (the receive) and the same polls are NAKed (nothing new) at
once; re-arm it and they fail again.  Control splits are unaffected, which is why
enumeration always worked.  So each keyboard/mouse round -- a few microframes every 8 ms --
runs with the receive paused (`usbe-rx-pause` / `usbe-rx-resume`, +0x12C): what has
arrived is taken first, the channel halted, and re-armed when the round ends.  The adapter
buffers meanwhile.

| | before | paused |
|---|---|---|
| HID transactions failed | all | **0** |
| NAKs (nothing new) | 0 | ~150/s |
| ping avg | 5 ms | **1-2 ms** |

`HFIR` also read 60000 (the full-speed frame interval) after a cold init; it is set for
the root port's speed now (7500 at high speed).  That was not the cause.

## The bus as a tree (net/usb-tree.lisp)

`usbt-enumerate` starts from a reset of the root port -- so U-Boot's state no longer
matters; after a netboot the RTL8153 used to come up three different ways (`ND:R`,
`ND:0`, `CDC:E2` then a binding that never received) -- and walks every hub at any depth,
one port reset at a time.  Each device record carries its TT: the first high-speed hub
above it and the port its branch leaves that hub by.  Then `usbt-bind-hid` binds the
first boot keyboard and mouse wherever they are, and `usbt-bind-nic` the first device
with a CDC-ECM configuration (the RTL8153's configuration 2; QEMU's `usb-net`).  The
packet filter must be set for the RTL8153 to receive in CDC-ECM (the old comment in
`r8152.lisp` saying ECM NAKs every bulk-IN was wrong for this sequence).

**A NIC BOUND BY CLASS NEEDS THE VENDOR DRIVER'S COPY HELPER.**  The engine copies every
received frame through `r8152-ncopy`'s native helper, which only the vendor probe
installed; bound by class, the engine core jumped to `0x100` on its first full-size frame
(found in QEMU; latent on the board, where `ssh-boot`'s early probe happened to install
it).  `usbt-bind-nic` calls `r8152-ensure-helpers`, which installs them behind a magic
word (that page holds garbage after a cold boot).

**HOTPLUG** (`usbt-scan`, the engine's hotplug service once the table is populated): every
hub's port changes; a disconnect forgets the branch below it and unbinds what was in it
(`NICX` when the NIC goes), a connect enumerates the new branch and binds what it finds.
Verified in QEMU by unplugging and replugging a keyboard behind a second hub, then the
hub itself.

`(modus-int::usbt-status)` lists the table.

The fault reporter prints `R` (X30, the caller) and `C` (MPIDR, which core) as well, since
a fault on core 1-3 halts that core alone.  `MODUS_SYMMAP=path` writes the symbol map at
build time; runtime address = map VA + `0x2FD80000`.

## Stage 4: recovery (USBE-RECOVER)

The engine's port check runs first in every pass.  When HPRT0 says the hub is connected
but the port is NOT enabled -- the end state of every root-port failure seen on this board
-- the bus is gone until a USB reset, and a reset of the hub resets everything behind it.
So recovery is the whole bus, cold, through the tree, and logged step by step
(`RCVR step a b` in kmsg):

| step | what | code |
|---|---|---|
| 0 | noticed (HPRT0, recovery count) | `usbe-port-service` |
| 1 | the whole tree enumerated again from a root-port reset | `usbt-enumerate` |
| 2 | keyboard and mouse bound | `usbt-bind-hid` |
| 3 | the NIC bound by its CDC-ECM configuration | `usbt-bind-nic` |
| 4 | the engine's interrupt routing, the receive armed | `usbe-irq-setup` |

The adapter comes back in CDC-ECM, not its vendor mode: the vendor mode needs the RTL8153
init U-Boot's r8152 driver does and modus does not have, while CDC-ECM is a standard class
-- kiln's `net-up` already drove the board's network this way.  The engine therefore speaks
both framings (`usbe-nic`: 2 = vendor aggregate, 1 = CDC-ECM one frame per transfer, 0 = no
NIC).  The network's own state -- address, ARP, TCP connections -- is untouched; TCP resends
what was lost.

Recovery conses (`hid-split-init` returns a list), so the engine leaves its GC-safe region
for it -- as any thread may -- and takes a new allocation baseline after.  Attempts are at
least 5 s apart, so a bus that will not come back costs a bring-up every 5 s, not a spin.

`(modus-int::usbe-test-disable-port)` disables the root port on purpose (PrtEna is
write-1-to-disable), which is the failure, reproduced on demand.

## The network on the I/O core (net/io-net.lisp, net/net-service.lisp, net/ssh-inetd.lisp)

**Core 0 is the Lisp system; core 1 is the I/O core.**  The network stack -- ARP, ICMP echo,
IPv4, TCP, UDP -- runs on core 1 as one more step of the engine's loop (`ion-step`), so a busy
evaluation on core 0 holds up only Lisp.  Measured on the Zero: 120 pings at 50 ms intervals
during a 31 s busy loop on the main thread, 120/120 answered, 0.27 ms average.

| core | role | rule |
|---|---|---|
| 0 | the Lisp system: main (REPL, desk, SSH evaluations), anything that compiles, interns or sets globals; inetd's accept threads, the watchdog, log shipping | may block; only Lisp waits |
| 1 | the I/O core: USB engine and network stack, one loop | never blocks, never allocates, never runs user code |
| 2-3 | SSH sessions (protocol and crypto), the desk's screen and audio | own heaps, talk by channel or through the static block |

**The rules of core 1 are the engine's**: no allocation, no heap pointer held, no global set,
no symbol, keyword or string literal; two-argument arithmetic only (`min`, `max` and
three-argument `logior` take `&rest` and allocate).  The engine's allocating-pass counter
stays 0 with the stack running.

**The static block** (`ion-base`, 0x11800000, 8 MB) is a region no collector owns.  It holds
words and bytes only -- no Lisp object, so nothing in it points anywhere -- and every core
reads and writes it.  Each word has one writer: a client allocates a connection, listener
or UDP port under the one lock (core 1 only ever *tries* it: a SYN that finds it held is
dropped and sent again); everything else is single-producer, single-consumer.  Clients wait
on one generation word core 1 bumps when anything changes.  **It survives a reset, and DRAM
decays during one**: a reset left `0xAAAAAAAA` records that core 1 then served as
connections, so the stack is on only if it was started in this boot (+0x60, as the engine's
+0x64).

**TCP data stays in the block until it is read.**  Each connection has a 64 KB receive ring
and a 64 KB send ring indexed by sequence number.  Core 1 copies a payload into the receive
ring once and the reader copies it out once; a writer copies into the send ring once and
core 1 segments -- and retransmits -- straight from it, so nothing is kept per segment.  The
advertised window is the receive ring's free space.  One measured necessity beyond in-order
receive: **data past one hole is kept** (written where it belongs, the range remembered).
The builder bursts up to 8 KB and a segment goes missing now and then; a capture showed what
dropping everything after the hole does -- duplicate ACKs, and the sender (no SACK here)
recovering one segment at a time.

**It stays out of keyboard and mouse transactions.**  A split's complete-split has a window
of a few microframes; a network pass of up to 1.5 ms in the middle of one made it miss --
99 failures over a 5 MB transfer.  `ion-step` does nothing during a round (the receive is
paused for it anyway) and ACKs once per connection per pass: 0 failures.

**The API** is `tcp-conns.lisp`'s, unchanged for its callers: `tcpc-open` / `-read` /
`-write` / `-close` / `-listen` / `-accept` and `udp-open` / `-send-to` / `-recv` /
`-close` delegate to `ion-*` once the stack is on, so `bare-sockets` (SB-BSD-SOCKETS) and kiln
work as before.  Loopback pairs stay in `tcp-conns`.  What core 1 does not take -- ARP
replies, broadcasts, DHCP, TCP for no connection of its own -- goes to a **legacy ring**
that `modus.net:nic-receive` reads, so `ip.lisp`'s one-connection TCP and its HTTP client
work unchanged (the installer's fetch uses it).

**Core 0's side** (`net-service.lisp`): inetd -- `(net-service-listen port spawn)` blocks a
core-0 thread on the port and hands each connection to the handler SPAWN starts, which
reads and writes it directly (no relay); evaluations on the main thread
(`net-service-eval`, drained from the console's wait); the watchdog (`PAUS` when core 0 is
held); log shipping.

**SSH** (`ssh-serve`) runs each session on core 2 or 3, its waits in real time (30 s for the
client's version, 60 s idle before authentication; ssh.lisp's 50-poll version wait was 0.5 s
and dropped any slower client right after its KEXINIT).  A session lets the client hang up
first: a FIN that overtook its DISCONNECT read as "closed by remote host".

**Evaluations run on the main thread**, one at a time, while it waits at the console for a
key -- as the old server's did, and as kiln relies on (`(kiln-zero-main)` over SSH takes the
main thread over).  Off the main thread, evaluation is wrong today: `(let ((s 0)) (dotimes
(i 3) (setq s (+ s 1))) s)` is 3 on main and 0 on a core-0 thread, and EVAL of a fresh form
on a thread wedged core 0 (open).  So an SSH command waits while main is busy.

Measured on the Zero (`ion-hw.sh`): SSH 6/6 one-shot, interactive, 4 concurrent; 4 MB in
in 1.1 s with a position-weighted checksum matching 3/3; 1 MB out in 0.5 s, byte-exact;
UDP in and out; ping 0.2-0.3 ms average; 0 retransmits, 0 keyboard/mouse failures, 0
allocating passes.

## The QEMU rig (/home/claude/qemu-zero)

QEMU 11.1.3 built locally (branch `zero-fidelity`) with what the Zero's bus does and stock
QEMU does not: TT split transactions with their timing (`dwc2-usb.split-model=on`,
`split-window=24` under TCG), a misrouted split answered with a transaction error, the
bulk-IN starvation above (`split-bulk-starve=on`), high-speed hubs (`usb-hub
high-speed=on`), a high-speed `usb-net`, and a vendor-class HDMI adapter stub
(`usb-display`).  The board console is the mini UART, QEMU's second serial
(`-serial null -serial stdio`).  The unfixed image fails every keyboard poll there; the
fixed one works.  Keyboard/mouse polls still fail now and then under TCG (`HIDF` in kmsg):
the emulator's timing, not the board's.
