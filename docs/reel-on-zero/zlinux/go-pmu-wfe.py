import serial, subprocess, sys, time
ser = serial.Serial('/dev/ttyAMA0', 115200, timeout=0.1); ser.reset_input_buffer()
def w(b):
    for i in range(len(b)): ser.write(b[i:i+1]); time.sleep(0.004)
def pump(d):
    e=time.time()+d; buf=b''
    while time.time()<e:
        x=ser.read(8192)
        if x: buf+=x; sys.stdout.write(x.decode('utf-8','replace')); sys.stdout.flush()
    return buf
subprocess.run(['bash','/home/modus/pi5-reset-zero.sh'], capture_output=True)
e=time.time()+9
while time.time()<e: w(b'\r'); ser.read(512); time.sleep(0.1)
w(b'setenv ipaddr 10.0.0.2\r'); pump(1); w(b'setenv serverip 10.0.0.1\r'); pump(1); w(b'usb start\r'); pump(12)
for _ in range(6):
    w(b'ping 10.0.0.1\r'); b=pump(8)
    if b'is alive' in b: break
    w(b'usb reset\r'); pump(14)
w(b'tftpboot 0x01000000 pmu-wfe.bin\r'); pump(15)
print('\n=== go 0x01000000 ===', flush=True)
w(b'go 0x01000000\r'); pump(6)
print('\n=== GO-PMU-WFE DONE ===', flush=True)
