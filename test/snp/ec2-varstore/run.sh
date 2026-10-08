#!/bin/bash
# Is EC2's UEFI variable store part of the SEV-SNP launch measurement?
#   AWS_REGION=us-east-2 test/snp/ec2-varstore/run.sh SOURCE_INSTANCE_ID
# 1. create-image from a running AL2023 SNP guest (so we own the snapshot)
# 2. register two AMIs from that ONE snapshot, identical except --uefi-data:
#      control: no variable store       vars: uefi-data-modus.txt (our KEK + db, NO PK:
#      Secure Boot stays in setup mode, so the image boots exactly as before)
# 3. launch control, control, vars -- all c6a.large SNP guests -- and print the ssh lines.
# Then take a report on each (configfs-tsm) and compare the MEASUREMENT field:
#   control == control  (the measurement is stable at all)
#   control != vars     => the variable store IS measured -> Secure Boot with our keys is attestable
#   control == vars     => it is not -> EC2 SNP cannot vouch for anything past AWS's firmware
set -euo pipefail
SRC=${1:?source instance id}; cd "$(dirname "$0")"
say() { echo "[varstore] $*" >&2; }
IMG=$(aws ec2 create-image --instance-id "$SRC" --no-reboot --name "modus-snp-src-$(date +%s)" --query ImageId --output text)
say "source image $IMG, waiting until available"; aws ec2 wait image-available --image-ids "$IMG"
SNAP=$(aws ec2 describe-images --image-ids "$IMG" --query 'Images[0].BlockDeviceMappings[?Ebs].Ebs.SnapshotId | [0]' --output text)
DEV=$(aws ec2 describe-images --image-ids "$IMG" --query 'Images[0].RootDeviceName' --output text)
say "snapshot $SNAP on $DEV"
reg() { aws ec2 register-image --name "$1-$(date +%s)" --architecture x86_64 --boot-mode uefi --ena-support \
          --virtualization-type hvm --root-device-name "$DEV" \
          --block-device-mappings "DeviceName=$DEV,Ebs={SnapshotId=$SNAP,VolumeType=gp3}" "${@:2}" --query ImageId --output text; }
CTL=$(reg modus-snp-control); VARS=$(reg modus-snp-vars --uefi-data "$(cat uefi-data-modus.txt)")
say "control AMI $CTL   vars AMI $VARS"; aws ec2 wait image-available --image-ids "$CTL" "$VARS"
for pair in "control-1:$CTL" "control-2:$CTL" "vars:$VARS"; do
  tag=${pair%%:*}; ami=${pair#*:}
  out=$(SNP=1 AMI=$ami ../../nitro/aws-launch.sh modus-nitro c6a.large 2>&1)
  echo "$tag: $(echo "$out" | grep -E '^instance:' | awk '{print $2}')  $(echo "$out" | grep -E '^ssh ' | awk '{print $NF}')"
done
