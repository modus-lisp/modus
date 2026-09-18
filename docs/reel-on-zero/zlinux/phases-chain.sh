#!/bin/bash
# repack initrd4 (new CLI + PMU harness), netboot Linux on the Zero, run per-phase counters. Log: phases.log
S=/tmp/claude-1002/-home-claude-modus/9e874ffc-83a3-493c-9640-e84a344b598b/scratchpad
cd $S/zlinux/root && find . | cpio -o -H newc 2>/dev/null | gzip -6 > $S/zlinux/initrd4.gz && ls -la $S/zlinux/initrd4.gz | cut -c24-
scp -q $S/zlinux/initrd4.gz modus@modus-pi:/home/modus/initrd4.gz && ssh modus@modus-pi 'sudo -n cp /home/modus/initrd4.gz /srv/tftp/initrd4.gz && sudo -n chown modus /srv/tftp/initrd4.gz && ls -la /srv/tftp/initrd4.gz | cut -c24-' < /dev/null
echo "=== NETBOOT"; timeout 400 ssh modus@modus-pi 'sudo -n python3 /home/modus/netboot-linux.py --initrd initrd4.gz' < /dev/null 2>&1 | grep -aE "===|NET UP|perf_user|Kernel|FAILED" | tail -8
echo "=== SHELL WAIT"; for i in $(seq 1 30); do r=$(timeout 12 ssh modus@modus-pi 'printf "echo READY; exit\n" | timeout 8 nc -w 4 10.0.0.2 2323' < /dev/null 2>/dev/null); [ "$r" = "READY" ] && break; sleep 4; done; echo "shell: $r"
echo "=== PHASES"; timeout 1500 ssh modus@modus-pi 'printf "cat /proc/sys/vm/overcommit_memory; cd /reel; modus --load /reel/modus-pmu-phases.lisp --quit 2>&1 | grep -avE \"^  WARN\"; exit\n" | timeout 1400 nc -w 1200 10.0.0.2 2323' < /dev/null 2>&1
echo "=== CHAIN DONE"
