import socket,time,re,random
s=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM); s.connect("q.sock"); s.settimeout(0.3)
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
    v=m.group(1) if m else None; print("  %-50s => %s"%(form[:50],v if v is not None else out.strip()[-80:]),flush=True); return v
print("  banner:", "Modus CL REPL" in rd_until(r"Modus CL REPL", 240.0), flush=True); time.sleep(2)
rd_until(r"never",1.5); s.sendall(b")))))))))\r\n(+ 0 0)\r\n"); rd_until(r"never",1.5)
q("(+ 20 22)")
q("(setq *jit-on* t)"); v=q("*jit-on*")
if v!="T": print("JIT NOT ON — abort"); raise SystemExit(1)
q("(setq *jit-hot-only* nil)")
q("(setq *tar-block-size* 512)")
if q('(if (find-package "REEL") 1 0)')!="1":
    q("(if (fboundp (quote ramv)) 1 0)")
    s.sendall(b"(defun ramv (a n) (let ((v (make-array n :element-type (quote (unsigned-byte 8))))) (dotimes (i n) (setf (aref v i) (mem-ref (+ a i) :u8))) v))\r\n"); rd_until(r"never",2.0)
    q("(progn (setq *rh-tarv* (ramv 436207616 143360)) (length *rh-tarv*))",300.0)
    print("=== install (JIT on) ===",flush=True); t0=time.time()
    q("(install-tarball-from-bytes *rh-tarv*)",5400.0)
    print("  install took %.0fs"%(time.time()-t0),flush=True)
q('(if (find-package "REEL") 1 0)')
q("(if (fboundp (quote reel::make-decoder)) 1 0)")
print("=== INSTALL PHASE DONE ===",flush=True)
