#!/bin/bash
# setup-pizero-gadget.sh — Configure Pi Zero 2 W as USB gadget for T420 development
#
# Run this on the Pi Zero 2 W after first boot with Raspberry Pi OS Lite.
# The Pi serves a Modus kernel image as a USB mass storage device.
#
# Prerequisites:
#   1. Flash Raspberry Pi OS Lite to SD card (use rpi-imager, enable SSH + WiFi)
#   2. Boot Pi Zero 2 W, SSH in over WiFi
#   3. Run this script: sudo bash setup-pizero-gadget.sh
#
# After setup:
#   - Copy image:  scp /tmp/modus-i386-diag-ssh.img pi@<pi-ip>:/home/pi/modus.img
#   - Start gadget: sudo /home/pi/start-gadget.sh
#   - Plug Pi USB into T420 → T420 boots from Pi
#   - Rebuild:  scp new-image pi@<pi-ip>:/home/pi/modus.img && T420 reboot

set -e

echo "=== Pi Zero 2 W USB Gadget Setup ==="

# 1. Enable dwc2 overlay (USB gadget mode)
if ! grep -q "dtoverlay=dwc2" /boot/firmware/config.txt 2>/dev/null; then
    # Try both paths (older vs newer Pi OS)
    CFG="/boot/firmware/config.txt"
    [ -f "$CFG" ] || CFG="/boot/config.txt"
    echo "dtoverlay=dwc2" >> "$CFG"
    echo "Added dwc2 overlay to $CFG"
fi

# 2. Load modules at boot
if ! grep -q "dwc2" /etc/modules; then
    echo "dwc2" >> /etc/modules
    echo "libcomposite" >> /etc/modules
    echo "Added dwc2 + libcomposite to /etc/modules"
fi

# 3. Create the gadget start script
cat > /home/pi/start-gadget.sh << 'GADGET_EOF'
#!/bin/bash
# Start USB mass storage gadget serving modus.img
# Usage: sudo ./start-gadget.sh [image-path]
set -e

IMG="${1:-/home/pi/modus.img}"
if [ ! -f "$IMG" ]; then
    echo "Error: $IMG not found. Copy it first:"
    echo "  scp /tmp/modus-i386-diag-ssh.img pi@$(hostname -I | awk '{print $1}'):/home/pi/modus.img"
    exit 1
fi

# Load modules
modprobe libcomposite 2>/dev/null || true

# Remove existing gadget if any
if [ -d /sys/kernel/config/usb_gadget/modus ]; then
    echo "" > /sys/kernel/config/usb_gadget/modus/UDC 2>/dev/null || true
    rm -rf /sys/kernel/config/usb_gadget/modus
fi

# Create gadget
cd /sys/kernel/config/usb_gadget
mkdir -p modus && cd modus

# USB IDs (generic mass storage)
echo 0x1d6b > idVendor   # Linux Foundation
echo 0x0104 > idProduct  # Multifunction Composite Gadget
echo 0x0100 > bcdDevice
echo 0x0200 > bcdUSB

# Strings
mkdir -p strings/0x409
echo "modus-dev" > strings/0x409/serialnumber
echo "Modus"     > strings/0x409/manufacturer
echo "Modus Dev" > strings/0x409/product

# Mass storage function
mkdir -p functions/mass_storage.0
echo 1 > functions/mass_storage.0/stall
echo 1 > functions/mass_storage.0/lun.0/removable
echo 0 > functions/mass_storage.0/lun.0/cdrom
echo "$IMG" > functions/mass_storage.0/lun.0/file

# Configuration
mkdir -p configs/c.1/strings/0x409
echo "Mass Storage" > configs/c.1/strings/0x409/configuration
echo 250 > configs/c.1/MaxPower
ln -sf functions/mass_storage.0 configs/c.1/

# Activate
UDC=$(ls /sys/class/udc | head -1)
echo "$UDC" > UDC

echo "USB gadget active: serving $IMG"
echo "Plug Pi USB into T420 and boot."
GADGET_EOF
chmod +x /home/pi/start-gadget.sh

# 4. Create stop script
cat > /home/pi/stop-gadget.sh << 'STOP_EOF'
#!/bin/bash
# Stop USB gadget (needed before updating the image file)
if [ -d /sys/kernel/config/usb_gadget/modus ]; then
    echo "" > /sys/kernel/config/usb_gadget/modus/UDC 2>/dev/null || true
    rm -f /sys/kernel/config/usb_gadget/modus/configs/c.1/mass_storage.0
    rmdir /sys/kernel/config/usb_gadget/modus/configs/c.1/strings/0x409 2>/dev/null
    rmdir /sys/kernel/config/usb_gadget/modus/configs/c.1 2>/dev/null
    rmdir /sys/kernel/config/usb_gadget/modus/functions/mass_storage.0 2>/dev/null
    rmdir /sys/kernel/config/usb_gadget/modus/strings/0x409 2>/dev/null
    rmdir /sys/kernel/config/usb_gadget/modus 2>/dev/null
    echo "Gadget stopped."
else
    echo "No gadget running."
fi
STOP_EOF
chmod +x /home/pi/stop-gadget.sh

# 5. Create update helper (stop gadget, copy image, restart)
cat > /home/pi/update-modus.sh << 'UPDATE_EOF'
#!/bin/bash
# Quick update: stop gadget, replace image, restart
# Run on Pi after scp'ing new image
set -e
sudo /home/pi/stop-gadget.sh
echo "Gadget stopped. Copy new image now, then run: sudo /home/pi/start-gadget.sh"
UPDATE_EOF
chmod +x /home/pi/update-modus.sh

echo ""
echo "=== Setup complete! ==="
echo ""
echo "Next steps:"
echo "  1. Reboot Pi:  sudo reboot"
echo "  2. SSH back in"
echo "  3. Copy image:  scp /tmp/modus-i386-diag-ssh.img pi@<pi-ip>:/home/pi/modus.img"
echo "  4. Start:       sudo /home/pi/start-gadget.sh"
echo "  5. Plug Pi USB into T420, boot T420"
echo ""
echo "To update image:"
echo "  1. Copy the new image to pi@<pi-ip>:/home/pi/modus.img"
echo "  2. On Pi:       sudo /home/pi/stop-gadget.sh && sudo /home/pi/start-gadget.sh"
echo "  3. Reboot T420"
