#!/bin/bash
set -e
R=/home/modus/initrd; rm -rf $R; mkdir -p $R/bin $R/usr/bin $R/lib/aarch64-linux-gnu $R/usr/lib/sbcl $R/dev $R/proc $R/sys $R/reel/decode $R/tmp
cp /usr/bin/busybox $R/bin/busybox
for c in sh mount cat echo sleep ls grep nproc free head; do ln -sf busybox $R/bin/$c; done
cp /usr/bin/sbcl $R/usr/bin/sbcl; cp /usr/lib/sbcl/sbcl.core $R/usr/lib/sbcl/sbcl.core
for b in /usr/bin/sbcl /usr/bin/busybox; do ldd $b | awk '/=> \//{print $3}' | while read l; do cp -L "$l" $R/lib/aarch64-linux-gnu/; done; done
cp -L /lib/ld-linux-aarch64.so.1 $R/lib/ld-linux-aarch64.so.1
cp /home/modus/reelbench/src/packages.lisp $R/reel/; cp /home/modus/reelbench/src/decode/*.lisp $R/reel/decode/
cp /home/modus/cam.ivf /home/modus/vp8-std.ivf /home/modus/bench.lisp $R/reel/
cp /home/modus/init $R/init; chmod +x $R/init $R/bin/busybox $R/usr/bin/sbcl
(cd $R && find . | cpio -o -H newc 2>/dev/null | gzip -6 > /home/modus/initrd.gz)
ls -la /home/modus/initrd.gz | cut -c24-; du -sh $R | cut -f1
sudo -n cp /home/modus/initrd.gz /srv/tftp/initrd.gz; sudo -n cp /boot/firmware/kernel8.img /srv/tftp/linux.img.gz; sudo -n chown modus /srv/tftp/initrd.gz /srv/tftp/linux.img.gz
ls -la /srv/tftp/initrd.gz /srv/tftp/linux.img.gz | cut -c24-; rm -rf $R; df -h / | tail -1
