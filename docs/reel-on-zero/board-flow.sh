#!/bin/bash
# netboot -> (SSH, NIC alive, DEFAULT hot-only) install reel + all forms + fetch clip -> serial-measure.py
IMG=$1; TAR=${2:-reel-neon.tar}; CLIP=${3:-cam.ivf}; cd /home/modus
pgrep -af "[h]ttp.server 8099" >/dev/null || { (nohup python3 -m http.server 8099 >http8099.log 2>&1 &); sleep 1; }
timeout 560 python3 netboot-gz.py --img "$IMG" --tftp-tries 6 --send-delay 240 --send '(ssh-boot)' > nb-flow.log 2>&1; echo "netboot exit=$?"
tr -d '\0' < nb-flow.log | grep -aE "Device NOT ready|TFTP FAILED|NET-PIPELINE|NETUP|NETBOOT-DONE" | sort | uniq -c
tr -d '\0' < nb-flow.log | grep -q NETUP || { echo "NO NETUP — abort"; exit 2; }
for i in $(seq 1 30); do ping -c1 -W1 10.0.0.2 >/dev/null 2>&1 && { echo "PING ok"; break; }; sleep 2; done
SSH="ssh -n -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o ConnectTimeout=20 -o PreferredAuthentications=password -o PubkeyAuthentication=no test@10.0.0.2"
run() { echo "--- $(echo "$1" | cut -c1-60)"; timeout ${2:-60} $SSH "$1" 2>/dev/null </dev/null | tr -d '\0\r' | grep -aE "^= " | cut -c1-160; }
run '(+ 2 3)'
run "(net-install-and-call \"http://10.0.0.1:8099/$TAR\")" 1800     # DEFAULT hot-only: trampolines, fast
run '(if (find-package "REEL") 1 0)'
while IFS= read -r f; do [ -n "$f" ] && run "$f" 120; done < demo-forms.txt | grep -c "^= "
while IFS= read -r f; do [ -n "$f" ] && run "$f" 120; done < hvs-all-forms.txt | grep -c "^= "
while IFS= read -r f; do [ -n "$f" ] && run "$f" 120; done < reel-hvs-forms.txt | grep -c "^= "
run '(init)' 300
run "(reel-demo-load \"http://10.0.0.1:8099/$CLIP\")" 300
run "(rh-load \"http://10.0.0.1:8099/$CLIP\")" 300
echo "=== switching to serial ==="
python3 -u /home/modus/serial-measure.py
