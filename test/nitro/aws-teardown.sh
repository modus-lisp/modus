#!/bin/bash
# aws-teardown.sh -- remove everything test/nitro/aws-launch.sh and test/snp/ec2-varstore/run.sh created.
#   AWS_REGION=us-east-2 test/nitro/aws-teardown.sh          (dry run: lists what it would delete)
#   AWS_REGION=us-east-2 test/nitro/aws-teardown.sh --yes    (deletes)
# Found by NAME, never by guess: instances tagged Name=modus-nitro|modus-snp, AMIs named
# modus-snp-*, the snapshots behind them, the VPC tagged Name=modus-nitro with its subnets,
# internet gateway and the modus-nitro-ssh security group, and the key pair modus-nitro.
set -uo pipefail
GO=${1:-}; export AWS_REGION=${AWS_REGION:-$(aws configure get region)}
say() { echo "[teardown $AWS_REGION] $*"; }
do_() { if [ "$GO" = --yes ]; then "$@" >/dev/null && echo "   done: $*" || echo "   FAILED: $*"; else echo "   would: $*"; fi; }

IDS=$(aws ec2 describe-instances --filters "Name=tag:Name,Values=modus-nitro,modus-snp" "Name=instance-state-name,Values=pending,running,stopping,stopped" --query 'Reservations[].Instances[].InstanceId' --output text)
say "instances: ${IDS:-none}"
if [ -n "$IDS" ]; then do_ aws ec2 terminate-instances --instance-ids $IDS
  [ "$GO" = --yes ] && { say "waiting for termination"; aws ec2 wait instance-terminated --instance-ids $IDS; }; fi

AMIS=$(aws ec2 describe-images --owners self --filters "Name=name,Values=modus-snp-*" --query 'Images[].ImageId' --output text)
SNAPS=$(aws ec2 describe-images --owners self --filters "Name=name,Values=modus-snp-*" --query 'Images[].BlockDeviceMappings[].Ebs.SnapshotId' --output text | tr '\t' '\n' | sort -u | tr '\n' ' ')
say "AMIs: ${AMIS:-none}   snapshots: ${SNAPS:-none}"
for a in $AMIS; do do_ aws ec2 deregister-image --image-id $a; done
for s in $SNAPS; do do_ aws ec2 delete-snapshot --snapshot-id $s; done

VPC=$(aws ec2 describe-vpcs --filters Name=tag:Name,Values=modus-nitro --query 'Vpcs[0].VpcId' --output text)
if [ "$VPC" != None ] && [ -n "$VPC" ]; then
  say "VPC $VPC"
  for sg in $(aws ec2 describe-security-groups --filters Name=vpc-id,Values=$VPC Name=group-name,Values=modus-nitro-ssh --query 'SecurityGroups[].GroupId' --output text); do do_ aws ec2 delete-security-group --group-id $sg; done
  for sn in $(aws ec2 describe-subnets --filters Name=vpc-id,Values=$VPC --query 'Subnets[].SubnetId' --output text); do do_ aws ec2 delete-subnet --subnet-id $sn; done
  for gw in $(aws ec2 describe-internet-gateways --filters Name=attachment.vpc-id,Values=$VPC --query 'InternetGateways[].InternetGatewayId' --output text); do
    do_ aws ec2 detach-internet-gateway --internet-gateway-id $gw --vpc-id $VPC; do_ aws ec2 delete-internet-gateway --internet-gateway-id $gw; done
  do_ aws ec2 delete-vpc --vpc-id $VPC
else say "VPC: none tagged modus-nitro"; fi

aws ec2 describe-key-pairs --key-names modus-nitro >/dev/null 2>&1 && { say "key pair modus-nitro"; do_ aws ec2 delete-key-pair --key-name modus-nitro; } || say "key pair: none"
[ "$GO" = --yes ] || say "dry run -- rerun with --yes to delete.  (~/.ssh/modus-nitro.pem is yours to remove afterwards.)"
