#!/bin/bash
# build-turnkey-ssh.sh — turnkey Pi Zero 2 W SSH-REPL SD image, locked to YOUR key.
#
# Give it an Ed25519 SSH *public* key; it bakes that key into a bare-metal Modus
# image (real RFC 4252 public-key auth — only the holder of the matching private
# key gets in) and produces an SD-card image you flash and boot.
#
# Usage:
#   scripts/build-turnkey-ssh.sh ~/.ssh/id_ed25519.pub
#   scripts/build-turnkey-ssh.sh "ssh-ed25519 AAAAC3Nz... you@host"
#   ssh-keygen -y -f ~/.ssh/id_ed25519 | scripts/build-turnkey-ssh.sh
#
# Network is USB-gadget (the Zero has no Ethernet): after flashing, plug the
# Pi's USB data port into your computer and it appears as a USB Ethernet
# adapter at 10.0.0.2 (host side 10.0.0.1).  Then `ssh test@10.0.0.2`.
#
# Ed25519 only — the server's crypto is Ed25519; RSA/ECDSA keys are rejected.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
die() { echo "ERROR: $*" >&2; exit 1; }

# --- read the public key (file arg, literal arg, or stdin) ---
if [ "$#" -ge 1 ] && [ -f "$1" ]; then
  KEY_LINE="$(grep -m1 . "$1" || true)"
elif [ "$#" -ge 1 ]; then
  KEY_LINE="$*"
else
  KEY_LINE="$(head -1)"
fi
[ -n "${KEY_LINE:-}" ] || die "no SSH key given (pass a .pub file, a key string, or pipe one in)"

read -r KEYTYPE KEYBLOB _COMMENT <<<"$KEY_LINE"
[ "$KEYTYPE" = "ssh-ed25519" ] || \
  die "key type is '$KEYTYPE'; this demo supports ssh-ed25519 only (make one with: ssh-keygen -t ed25519)"
[ -n "${KEYBLOB:-}" ] || die "malformed key line: no base64 blob"

# --- extract the raw 32-byte key from the OpenSSH blob ---
# blob = string("ssh-ed25519") + string(32-byte key) = 4+11 + 4+32 = 51 bytes;
# the key is the last 32 bytes.
RAW="$(printf '%s' "$KEYBLOB" | base64 -d 2>/dev/null | tail -c 32 || true)"
[ "$(printf '%s' "$RAW" | wc -c)" -eq 32 ] || die "could not decode a 32-byte Ed25519 key from the blob"
HEX="$(printf '%s' "$RAW" | od -An -v -tx1 | tr -d ' \n')"
[ "${#HEX}" -eq 64 ] || die "internal: extracted key is ${#HEX} hex chars, expected 64"

echo "Authorized key (ed25519): ${HEX:0:16}…${HEX: -8}"
echo "Building turnkey SSH image…"

# --- build kernel + SD image (build-pizero2w.sh reads MODUS_SSH_AUTH_KEY_HEX
#     via build-pizero2w-ssh.lisp, then makes /tmp/pizero2w.img) ---
export MODUS_SSH_AUTH_KEY_HEX="$HEX"
"$SCRIPT_DIR/build-pizero2w.sh" --no-actors

cat <<EOF

=== Turnkey SSH REPL image ready ===
Image: /tmp/pizero2w.img   (locked to the ed25519 key above)

1. Flash it:
     sudo dd if=/tmp/pizero2w.img of=/dev/sdX bs=4M status=progress
2. Insert the SD card, plug the Pi's USB DATA port into your computer.
3. Bring up the host side of the USB link (interface name varies, e.g. usb0):
     sudo ip addr add 10.0.0.1/24 dev usb0
     sudo ip link set usb0 up
4. Log in with the MATCHING private key — no other key or password works:
     ssh test@10.0.0.2
   You get a Modus REPL:  (+ 1 2)  =>  3
EOF
