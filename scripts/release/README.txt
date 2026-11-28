modus @RELEASE@
==============

modus is a Common Lisp: a compiler, a runtime and an operating system, written in Lisp.
This folder is one release of it, for Linux (x86-64), Windows (x86-64) and macOS (Apple
silicon), with the source of every modus-lisp repository.

    modus-linux-x64          modus for Linux
    modus-windows-x64.exe    modus for Windows
    modus-macos-arm64        modus for macOS
    desk.sh  repl.sh         launchers for Linux
    desk.cmd repl.cmd        launchers for Windows
    desk.command repl.command  launchers for macOS (double-click them in Finder)
    src/                     the sources: modus/, glass/, cram/, ... one folder per repository

Run a launcher from this folder or by double-clicking it; each finds the rest beside itself.


repl
----

repl.sh, repl.cmd and repl.command start the modus REPL: type a form, see its value.
(sb-ext:exit) leaves.  Arguments go to modus as they are, so

    ./repl.sh --eval "(print (+ 1 2))" --quit
    ./repl.sh --script hello.lisp
    ./repl.sh --help

work as they do with SBCL.  (On Windows: repl --eval "(print (+ 1 2))" --quit.)


desk
----

desk.sh, desk.cmd and desk.command start the glass desk: a window manager, with a Lisp
listener, a file browser and the other desk apps, loaded from src/ (the first start
takes a few minutes while it compiles) and served

  - to a web browser, at http://127.0.0.1:6080/ (it opens one for you), and
  - over VNC, on port 5910 (any VNC viewer: 127.0.0.1::5910).

It listens on 127.0.0.1 only, unless you say otherwise.  Its flags:

    --port N          the VNC port (default 5910)
    --web-port N      the web port (default 6080)
    --no-web          no web port
    --no-open         do not open a web browser
    --address ADDR    the interface to listen on (0.0.0.0: every one)
    --size WxH        the screen (default 1024x700)

and more: run   ./desk.sh --help   (or desk --help, ./desk.command --help) for the list.
The desk uses up to 8 GB of address space (--dynamic-space-size 8192); it does not need
that much memory.


macOS: "cannot be opened" / "unidentified developer"
----------------------------------------------------

Files downloaded by a browser are quarantined, and modus-macos-arm64 is not notarized by
Apple, so the first time macOS may refuse to run a launcher or modus itself.  Either

  - in Finder, right-click (or Control-click) desk.command, choose Open, then Open again
    in the dialog; do the same for modus-macos-arm64 if it is refused next; or
  - in Terminal, in this folder, clear the quarantine once for everything here:

        xattr -dr com.apple.quarantine .

    (or for one file:  xattr -d com.apple.quarantine modus-macos-arm64)


Windows: "Windows protected your PC"
------------------------------------

SmartScreen may stop a launcher the first time.  Choose "More info", then "Run anyway".


The sources
-----------

src/ holds every modus-lisp repository at the commits this release was built from, as
`git archive` exports them, less images, video and PDFs.  The desk loads its systems from
here through ASDF, and nothing from the network.  Building modus itself from them is
described in src/modus/docs/building.md.
