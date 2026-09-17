#!/bin/bash
cd /home/modus; TAR=${1:-reel-neon.tar}; CLIP=${2:-cam.ivf}
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
