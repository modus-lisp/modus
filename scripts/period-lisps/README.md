# Period Lisps under MAME

Runs Macintosh Common Lisp 2.0 and Pearl Lisp 1.1 (and, optionally, Allegro CL 1.2.2,
which crashes at launch) on emulated Macs, next to the Modus application, and types
forms at their Listeners. Nothing here is a ROM, a System or a Lisp. The inputs are the
user's uploads plus public downloads, and everything built goes under
`$LISPS` (default `/mnt/data-fast/claude/rss/lisps`).

| file | what |
|---|---|
| `build-disks.sh [pearl mcl acl]` | fetches System 6.0.8 (archive.org `macintosh-system-6.0.8-hd.tar`), Debian 12's `unar` (into a private prefix) and `machfs` (into a venv), unpacks the uploads, and builds `disks/WHICH.hd.chd` (System 6.0.8 plus one Lisp, launched at startup) |
| `mcl-join-cpt.py` | joins the three-segment Compact Pro archive `MCL 2.0.sea` / `.#2` / `.#3` (on MCL floppies 1-3) into one archive `unar` can extract |
| `ndif.py`, `adouble.py` | Disk Copy 6 NDIF (ADC) decoder for the Allegro disks; AppleDouble reader |
| `mkvol.py APPS OUT.hfv [MB] [START]` | the System volume: `APPS` is a comma list of `pearl,mcl,acl,modus` and `START` picks the one launched at boot. `SYSTEM=753` uses the System Folder of the uploaded System 7.5.3 image instead of 6.0.8, with START's application copied into Startup Items. `MODUS_FORKS=DIR` names the Modus application (`scripts/mac68k-chain.py DISK OUT.bin --forks DIR`) |
| `lisp.lua` | MAME autoboot driver. It waits `DELAY` emulated seconds, then types each `|`-separated form of `MAC_FORMS` plus Return, `GAP` seconds apart, and saves `SNAPDIR/shot-NN.png` before each form and once at the end |
| `run-lisp.sh WHICH OUTDIR [MACHINE] [RAM]` | boots `disks/WHICH.hd.chd` (or `DISK=`) and types `bench-forms.lisp`, each form wrapped as `(time (eval (quote F)))` (`TIMED=0` to send them as is; `MAC_FORMS=` to override). `shot-(k+1)` shows form k's result |
| `bench-forms.lisp` | the timing forms |

## Rebuild

```sh
scripts/period-lisps/build-disks.sh pearl mcl           # ~1 min, needs network
scripts/period-lisps/run-lisp.sh mcl /tmp/x macclasc 4M  # then Read /tmp/x/shot-*.png
```

MCL and Modus on one volume (System 6.0.8 for the Classic, 7.5.3 for the Quadra 950;
6.0.8 refuses a 68040 with "System 6.0.8 does not work on this model"):

```sh
L=/mnt/data-fast/claude/rss/lisps; V="$L/venv/bin/python -I"
python3 scripts/mac68k-chain.py /tmp/rss/m-n68j/c.dsk $L/modus/n68j/Modus.bin --forks $L/modus/n68j
MODUS_FORKS=$L/modus/n68j $V scripts/period-lisps/mkvol.py mcl,modus mm.hfv 24 modus
SYSTEM=753 MODUS_FORKS=$L/modus/q3 $V scripts/period-lisps/mkvol.py mcl,modus mmq.hfv 48 mcl
sbcl --script scripts/mac68k-hd.lisp mm.hfv mm.hd --chd "env LD_LIBRARY_PATH=/tmp/rss/mame/root/usr/lib/x86_64-linux-gnu /tmp/rss/mame/root/usr/bin/chdman"
```

`macqd950` is not in MAME 0.251. `run-lisp.sh` uses the Quadra-only 0.289 build at
`/mnt/data-fast/claude/rss/msrc/mame/qd` for `macqd*` machines.

## Gotchas

- The MCL floppies are Disk Copy 4.2 images: drop the 84-byte header and you have a raw
  800K HFS disk. The Compact Pro segments each begin with an 8-byte header. Only the last
  segment's header points at the directory, and the directory's file offsets are
  (volume, offset in that segment). The join rebases them and recomputes the directory
  CRC as Compact Pro stores it: CRC-32 without the final inversion.
- System 7's Finder ignores the volume's "Set Startup" information, so `mkvol.py`
  puts a copy of the start application in Startup Items.
- Pearl Lisp has no `INCF` and no `GET-INTERNAL-REAL-TIME`. Its `TIME` reports ticks.
- On the Quadra 950 with System 7.5.3, the keystrokes of the first form are lost even
  with `DELAY=150`. Send a throwaway `nil` first.
- On the emulated 68040, MCL 2.0's `TIME` prints the right milliseconds but the wrong
  seconds in parentheses: `(FIB 25) took 1470 milliseconds (64.735 seconds)`. The
  milliseconds agree with `GET-UNIVERSAL-TIME` (10 x (fib 25): 14701 ms internal, 14 s
  universal). On the Classic the two figures agree.
- PRAM: `$LISPS/nvram/q950-32bit` is the Quadra 950's Egret PRAM with byte `$8A` set to
  `$25` (32-bit addressing on), for `NVRAM=`. MAME's fresh PRAM has 24-bit addressing
  (BufPtr near 8 MB). `$20` alone does not turn it on.
- Modus, `scripts/mac68k-chain.py`'s application, stops on System 7.5.3 / Quadra 950 with
  "Modus: not enough memory (it needs 4 MB here)" in both addressing modes. Its check
  reads low-memory MemTop (`$108`), which System 7 sets per application partition. With
  MCL running it read `$001B13E0`, not 32 MB.
- MAME ignores SIGTERM while running headless; stop a run with `kill -9`.
