#!/bin/bash
# snp-kit.sh IMAGEDIR OVMF.fd OUT.tar.gz -- bundle what the rented SNP box needs:
# the kiln image dir's generic.efi + manifest, the AmdSev OVMF, the expected launch
# measurement for each plausible vCPU model (sev-snp-measure), the bring-up script
# and the SNP test tools.  Everything measured here, before any money is spent.
set -eu
IMG=$1; OVMF=$2; OUT=$3; cd "$(dirname "$0")/.."
K=$(mktemp -d); mkdir -p $K/kit/test/snp $K/kit/scripts
cp "$IMG/generic.efi" "$IMG/manifest.json" $K/kit/; cp "$OVMF" $K/kit/OVMF.fd
cp test/snp/attested-ssh-client.sh test/snp/verify-report.py test/snp/kds-fetch.py test/snp/amd-milan-chain.pem $K/kit/test/snp/
cp scripts/snp-host-bringup.sh $K/kit/scripts/; cp scripts/snp-host-bringup.sh $K/kit/bringup.sh
python3 - "$K/kit" <<'PY'
import json, subprocess, sys, os
kit = sys.argv[1]; out = {}
for cpu in ("EPYC-v4", "EPYC-Milan", "EPYC-Milan-v2", "EPYC-Genoa", "EPYC-Turin", "EPYC-Rome"):
    r = subprocess.run([os.path.expanduser("~/.local/bin/sev-snp-measure"), "--mode", "snp", "--vcpus", "1", "--vcpu-type", cpu, "--vmm-type", "QEMU",
                        "--ovmf", kit + "/OVMF.fd", "--kernel", kit + "/generic.efi", "--append", ""], capture_output=True, text=True)
    out[cpu] = r.stdout.strip() if r.returncode == 0 else "ERR " + r.stderr.strip()[-80:]
json.dump(out, open(kit + "/expected-measurement.json", "w"), indent=1)
for k, v in out.items(): print(f"   {k}: {v[:32]}..")
PY
( cd $K && tar czf - kit ) > "$OUT"; echo "wrote $OUT $(stat -c %s "$OUT") bytes"; rm -rf $K
