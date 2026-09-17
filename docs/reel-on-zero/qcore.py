import socket,time,re,random,subprocess,sys
S="/tmp/claude-1002/-home-claude-modus/9e874ffc-83a3-493c-9640-e84a344b598b/scratchpad"
s=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM); s.connect(S+"/q.sock"); s.settimeout(0.3)
def rd_until(pat,to):
    e=time.time()+to; b=b""
    while time.time()<e:
        try:
            d=s.recv(65536)
            if d: b+=d
        except socket.timeout: pass
        if re.search(pat,b.decode("utf-8","replace")): break
    return b.decode("utf-8","replace")
def q(form,to=10.0):
    t=random.randint(10000,99999); s.sendall(("(list %d %s)\r\n"%(t,form)).encode())
    out=rd_until(r"\(%d .+?\)\s*\n"%t,to); m=re.search(r"\(%d (.+?)\)\s*\n"%t,out+"\n")
    v=m.group(1) if m else None; print("  %-56s => %s"%(form[:56],v if v is not None else out.strip()[-100:]),flush=True); return v
def push(path,to=60.0):
    n=0
    for line in open(path):
        line=line.strip()
        if not line: continue
        s.sendall((line+"\r\n").encode()); rd_until(r"\n> ",to); n+=1
    print("  pushed %d forms from %s"%(n,path.split("/")[-1]),flush=True)
print("banner:", "Modus CL REPL" in rd_until(r"Modus CL REPL",1500.0), flush=True); time.sleep(2)
rd_until(r"never",1.5); s.sendall(b")))))))))\r\n(+ 0 0)\r\n"); rd_until(r"never",1.5)
q("(+ 20 22)"); q("(setq *jit-on* t)"); q("*jit-hot-only*"); q("*jit-linkage-cells*")
q("(setq *tar-block-size* 512)")
s.sendall(b"(defun ramv (a n) (let ((v (make-array n :element-type (quote (unsigned-byte 8))))) (dotimes (i n) (setf (aref v i) (mem-ref (+ a i) :u8))) v))\r\n"); rd_until(r"never",2.0)
q("(progn (setq *rh-tarv* (ramv 436207616 163840)) (length *rh-tarv*))",300.0)
q("(list (aref *rh-tarv* 0) (aref *rh-tarv* 1) (aref *rh-tarv* 2))")   # 114 101 101 = 'ree'
print("=== install (TCG, minutes) ===",flush=True); t0=time.time()
q("(install-tarball-from-bytes *rh-tarv*)",5400.0); print("  install %.0fs"%(time.time()-t0),flush=True)
q('(if (find-package "REEL") 1 0)'); q("(if (fboundp (quote reel::make-decoder)) 1 0)"); q("(if (fboundp (quote reel.decode::%edge-sub-h8)) 1 0)")
push(S+"/demo-forms.txt"); push(S+"/hvs-all-forms.txt"); push(S+"/reel-hvs-forms.txt")
q("(if (fboundp (quote reel-demo-pass)) 1 0)"); q("(if (fboundp (quote rh-play)) 1 0)")
q("(progn (setq *rh-ivf* (ramv 452984832 151295) *rh-len* 151295 *demo-ivf* *rh-ivf* *demo-len* 151295) *rh-len*)",300.0)
q("(list (aref *rh-ivf* 0) (aref *rh-ivf* 1) (aref *rh-ivf* 2) (aref *rh-ivf* 3))")   # 68 75 73 70 = DKIF
q("(setq *rh-tarv* nil)")
print("=== jit-eager ===",flush=True); t0=time.time(); q("(setq *jit-hot-only* nil)"); q("(jit-eager)",3600.0); print("  jit-eager %.0fs"%(time.time()-t0),flush=True)
q("(list *jit-native-defun-count* (%jit-fn-native-p \"DECODE-FRAME\") (%jit-fn-native-p \"MC-FILTER\"))")
print("=== save ===",flush=True)
s.sendall(b'(%save-image "reel-lc.core")\r\n'); out=rd_until(r"CORE-END=\d+",1800.0)
m=re.search(r"CORE-END=(\d+)",out)
if not m: print("NO CORE-END:",out[-300:]); sys.exit(1)
end=int(m.group(1)); print("  core 0x18000000..%#x (%.1f MB)"%(end,(end-0x18000000)/1e6),flush=True)
g=subprocess.run(["gdb-multiarch","-q","-batch","-ex","set architecture aarch64","-ex","target remote :1234",
   "-ex","dump binary memory %s/reel-lc.core 0x18000000 %d"%(S,end),"-ex","detach"],capture_output=True,text=True)
print("  gdb:",(g.stdout+g.stderr).strip()[-160:],flush=True)
print("=== QCORE DONE ===",flush=True)
