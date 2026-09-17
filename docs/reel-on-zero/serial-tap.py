import serial,sys,time
s=serial.Serial("/dev/ttyAMA0",115200,timeout=0.2); f=open(sys.argv[1],"ab")
while True:
    d=s.read(8192)
    if d: f.write(d); f.flush()
