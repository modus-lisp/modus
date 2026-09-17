import serial, subprocess, time, sys
s=serial.Serial("/dev/ttyAMA0",115200,timeout=0.2); s.reset_input_buffer()
def pump(d):
    e=time.time()+d; b=b""
    while time.time()<e:
        x=s.read(8192)
        if x: b+=x
    return b.decode("utf-8","replace").replace("\0","")
print("--- serial idle 1s:", repr(pump(1.0)[-120:]))
p=subprocess.Popen(["ssh","-n","-o","StrictHostKeyChecking=no","-o","UserKnownHostsFile=/dev/null","-o","ConnectTimeout=15",
  "-o","PreferredAuthentications=password","-o","PubkeyAuthentication=no","test@10.0.0.2","(+ 2 3)"],
  stdout=subprocess.PIPE,stderr=subprocess.STDOUT,stdin=subprocess.DEVNULL)
out=pump(25.0)
try: r=p.communicate(timeout=5)[0].decode("utf-8","replace")
except Exception: p.kill(); r="<ssh still running>"
print("--- SERIAL during ssh ---"); print(out[-1500:])
print("--- ssh client said ---"); print(r.strip()[-300:])
