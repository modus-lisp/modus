#!/usr/bin/env python3
"""romlink.py MAMECMD SET... -- give each SET's ROMs their MAME names, matched by CRC from /tmp/rss/romsrc."""
import os,re,subprocess,sys,zlib,shutil
mame=sys.argv[1].split(); sets=sys.argv[2:]
crc={}
for d,_,fs in os.walk('/tmp/rss/romsrc'):
    for f in fs:
        p=os.path.join(d,f)
        if os.path.getsize(p)<=4*1024*1024: crc.setdefault('%08x'%(zlib.crc32(open(p,'rb').read())&0xffffffff),p)
out=subprocess.run(mame+['-listroms']+sets,capture_output=True,text=True,env=dict(os.environ,HOME='/tmp/rss/mame')).stdout
cur=None
for line in out.splitlines():
    m=re.match(r'ROMs required for driver "(\w+)"',line)
    if m: cur=m.group(1); os.makedirs('/tmp/rss/mame/roms/'+cur,exist_ok=True); continue
    m=re.match(r'(\S+)\s+\d+\s+CRC\((\w+)\)',line)
    if m and cur:
        src=crc.get(m.group(2).lower())
        print(f'{cur}: {m.group(1)} <- {src}')
        if src: shutil.copy(src,f'/tmp/rss/mame/roms/{cur}/{m.group(1)}')
