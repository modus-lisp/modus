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

# A VPC with a public subnet.  Accounts without a default VPC get one built
# here, tagged modus-nitro so a second run reuses it (and teardown can find it).
VPC=$(aws ec2 describe-vpcs --filters Name=tag:Name,Values=modus-nitro --query 'Vpcs[0].VpcId' --output text)
if [ "$VPC" = None ] || [ -z "$VPC" ]; then
  VPC=$(aws ec2 describe-vpcs --filters Name=isDefault,Values=true --query 'Vpcs[0].VpcId' --output text)
fi
if [ "$VPC" = None ] || [ -z "$VPC" ]; then
  VPC=$(aws ec2 create-vpc --cidr-block 10.77.0.0/16 --tag-specifications 'ResourceType=vpc,Tags=[{Key=Name,Value=modus-nitro}]' --query Vpc.VpcId --output text)
  aws ec2 modify-vpc-attribute --vpc-id "$VPC" --enable-dns-hostnames
  IGW=$(aws ec2 create-internet-gateway --tag-specifications 'ResourceType=internet-gateway,Tags=[{Key=Name,Value=modus-nitro}]' --query InternetGateway.InternetGatewayId --output text)
  aws ec2 attach-internet-gateway --vpc-id "$VPC" --internet-gateway-id "$IGW"
  RTB=$(aws ec2 describe-route-tables --filters Name=vpc-id,Values=$VPC --query 'RouteTables[0].RouteTableId' --output text)
  aws ec2 create-route --route-table-id "$RTB" --destination-cidr-block 0.0.0.0/0 --gateway-id "$IGW" >/dev/null
  SUBNET=$(aws ec2 create-subnet --vpc-id "$VPC" --cidr-block 10.77.1.0/24 --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=modus-nitro}]' --query Subnet.SubnetId --output text)
  aws ec2 modify-subnet-attribute --subnet-id "$SUBNET" --map-public-ip-on-launch
  say "created VPC $VPC, subnet $SUBNET, igw $IGW"
else
  SUBNET=$(aws ec2 describe-subnets --filters Name=vpc-id,Values=$VPC Name=map-public-ip-on-launch,Values=true --query 'Subnets[0].SubnetId' --output text)
  [ "$SUBNET" = None ] && SUBNET=$(aws ec2 describe-subnets --filters Name=vpc-id,Values=$VPC --query 'Subnets[0].SubnetId' --output text)
fi
say "VPC $VPC subnet $SUBNET"

MYIP=$(curl -s https://checkip.amazonaws.com)/32
SGID=$(aws ec2 describe-security-groups --filters Name=group-name,Values=$SG Name=vpc-id,Values=$VPC --query 'SecurityGroups[0].GroupId' --output text)
if [ "$SGID" = None ] || [ -z "$SGID" ]; then
  SGID=$(aws ec2 create-security-group --group-name "$SG" --description "ssh for modus nitro host" --vpc-id "$VPC" --query GroupId --output text)
  aws ec2 authorize-security-group-ingress --group-id "$SGID" --protocol tcp --port 22 --cidr "$MYIP" >/dev/null
  say "security group $SGID, ssh from $MYIP"
fi

ID=$(aws ec2 run-instances \
  --image-id "$AMI" --instance-type "$TYPE" --key-name "$KEY" --security-group-ids "$SGID" --subnet-id "$SUBNET" --associate-public-ip-address \
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
(the modus-nitro VPC/subnet/igw/sg are free to keep; delete by tag Name=modus-nitro if you want them gone)
EOT
