#!/bin/bash
# Launch ONE Nitro-Enclaves-capable spot instance for the first modus.eif run.
#   test/nitro/aws-launch.sh [KEYNAME] [TYPE]      (defaults: modus-nitro, m5.xlarge)
# Needs: aws cli signed in (aws sts get-caller-identity), region set (AWS_REGION
# or `aws configure`).  Creates the key pair and a security group (SSH from this
# host's public IP only) if they do not exist.  Prints the instance id and the
# ssh line.  Tear down with:  aws ec2 terminate-instances --instance-ids <id>
set -euo pipefail
KEY=${1:-modus-nitro}; TYPE=${2:-m5.xlarge}; SG=modus-nitro-ssh
say() { echo "[aws-launch] $*" >&2; }

# Amazon Linux 2023 x86_64, resolved through SSM so no AMI id is pinned here.
AMI=$(aws ssm get-parameter --name /aws/service/ami-amazon-linux-latest/al2023-ami-kernel-default-x86_64 --query Parameter.Value --output text)
say "AMI $AMI"

if ! aws ec2 describe-key-pairs --key-names "$KEY" >/dev/null 2>&1; then
  aws ec2 create-key-pair --key-name "$KEY" --query KeyMaterial --output text > ~/.ssh/$KEY.pem
  chmod 600 ~/.ssh/$KEY.pem; say "key pair $KEY -> ~/.ssh/$KEY.pem"
fi

MYIP=$(curl -s https://checkip.amazonaws.com)/32
if ! SGID=$(aws ec2 describe-security-groups --group-names "$SG" --query 'SecurityGroups[0].GroupId' --output text 2>/dev/null); then
  SGID=$(aws ec2 create-security-group --group-name "$SG" --description "ssh for modus nitro host" --query GroupId --output text)
  aws ec2 authorize-security-group-ingress --group-id "$SGID" --protocol tcp --port 22 --cidr "$MYIP" >/dev/null
  say "security group $SGID, ssh from $MYIP"
fi

ID=$(aws ec2 run-instances \
  --image-id "$AMI" --instance-type "$TYPE" --key-name "$KEY" --security-group-ids "$SGID" \
  --enclave-options Enabled=true \
  --instance-market-options 'MarketType=spot,SpotOptions={SpotInstanceType=one-time,InstanceInterruptionBehavior=terminate}' \
  --block-device-mappings 'DeviceName=/dev/xvda,Ebs={VolumeSize=16,VolumeType=gp3}' \
  --tag-specifications 'ResourceType=instance,Tags=[{Key=Name,Value=modus-nitro}]' \
  --query 'Instances[0].InstanceId' --output text)
say "instance $ID, waiting for running"
aws ec2 wait instance-running --instance-ids "$ID"
IP=$(aws ec2 describe-instances --instance-ids "$ID" --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)
cat <<EOT
instance: $ID
ssh -i ~/.ssh/$KEY.pem ec2-user@$IP
then, on the host (docs/nitro-enclaves.md):
  sudo dnf install -y aws-nitro-enclaves-cli aws-nitro-enclaves-cli-devel && sudo usermod -aG ne ec2-user
  sudo sed -i 's/^memory_mib:.*/memory_mib: 3072/; s/^cpu_count:.*/cpu_count: 2/' /etc/nitro_enclaves/allocator.yaml
  sudo systemctl enable --now nitro-enclaves-allocator.service
  # log out and back in for the group, scp modus.eif + test/nitro/*.py up, then
  nitro-cli run-enclave --eif-path modus.eif --memory 3072 --cpu-count 2 --enclave-cid 16 --debug-mode
teardown: aws ec2 terminate-instances --instance-ids $ID
EOT
