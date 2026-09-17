#!/bin/bash
# Usage: board-measure.sh <image.img.gz in /home/modus> <reel.tar> <clip.ivf>
# Encodes docs/reel-on-zero/BOARD-RUNBOOK.md sections 2-6. Run ON modus-pi.
IMG=$1; TAR=${2:-reel-neon.tar}; CLIP=${3:-cam.ivf}
cd /home/modus || exit 1
echo "=== stage $IMG ==="
sudo -n cp "/home/modus/$IMG" /srv/tftp/ && sudo -n chown modus:modus "/srv/tftp/$IMG" && ls -la "/srv/tftp/$IMG" || { echo "STAGE FAILED"; exit 1; }
pgrep -af "[h]ttp.server 8099" >/dev/null || { (cd /home/modus && nohup python3 -m http.server 8099 >http8099.log 2>&1 &); sleep 1; }
echo "http: $(pgrep -af '[h]ttp.server 8099' | head -1 | cut -c1-50)"
ls -la "$TAR" "$CLIP" || { echo "MISSING TAR/CLIP"; exit 1; }
echo "=== netboot ==="
timeout 560 python3 /home/modus/netboot-gz.py --img "$IMG" --tftp-tries 6 --send-delay 240 --send '(ssh-boot)' > /home/modus/nb-measure.log 2>&1
echo "netboot exit=$?"
tr -d '\0' < /home/modus/nb-measure.log | grep -aE "tftpboot try|Bytes transferred|Uncompressed|TFTP FAILED|UNZIP|Modus CL|NETUP|UNDEFINED|FAULT|ESR|NETBOOT-DONE" | tail -10
tr -d '\0' < /home/modus/nb-measure.log | grep -q NETUP || { echo "NO NETUP — abort"; exit 2; }
for i in $(seq 1 30); do ping -c1 -W1 10.0.0.2 >/dev/null 2>&1 && { echo "PING ok"; break; }; sleep 2; done
SSH="ssh -n -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o ConnectTimeout=20 -o PreferredAuthentications=password -o PubkeyAuthentication=no test@10.0.0.2"
run() { echo "--- $(echo "$1" | cut -c1-70)"; timeout ${2:-60} $SSH "$1" 2>/dev/null </dev/null | tr -d '\0\r' | grep -aE "^= " | cut -c1-200; }
echo "=== install reel ==="
R0=$(timeout 60 $SSH '(+ 2 3)' 2>&1 </dev/null | tr -d '\0\r' | grep -aE '^= ' | head -1)
echo "--- (+ 2 3) => ${R0:-<no reply>}"
[ "$R0" = "= 5" ] || { echo "SSH DEAD (no '= 5') — abort; run ssh -vv / serial-ssh-diag.py"; exit 3; }
run '(setq *jit-hot-only* nil)'
run '(if (boundp (quote *jit-linkage-cells*)) *jit-linkage-cells* :unbound)'
run "(net-install-and-call \"http://10.0.0.1:8099/$TAR\")" 2400
run '(if (find-package "REEL") 1 0)'
echo "=== demo forms + measure ==="
while IFS= read -r form; do [ -n "$form" ] && run "$form" 300; done < /home/modus/demo-forms.txt
run '(init)' 300
run "(reel-demo-load \"http://10.0.0.1:8099/$CLIP\")" 300
for k in 1 2 3; do run '(reel-demo-pass 4)' 900; done
echo "=== MEASURE DONE ==="
