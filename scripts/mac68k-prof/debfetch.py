#!/usr/bin/env python3
"""debfetch.py SUITE DEST PKG... -- fetch PKG and its Depends not installed on this host, unpack into DEST/root."""
import lzma,re,subprocess,sys,os,urllib.request,concurrent.futures as cf
suite,dest,want=sys.argv[1],sys.argv[2],sys.argv[3:]
os.makedirs(dest+'/debs',exist_ok=True)
text=''
for comp in ('main','contrib'):
    pf=f'/tmp/rss/pk/Packages-{suite}-{comp}.xz'
    if not os.path.exists(pf):
        urllib.request.urlretrieve(f'https://deb.debian.org/debian/dists/{suite}/{comp}/binary-amd64/Packages.xz',pf)
    text+=lzma.open(pf).read().decode('utf-8','replace')+'\n\n'
pk={}; prov={}
for blk in text.split('\n\n'):
    f={}
    for m in re.finditer(r'^([A-Za-z-]+): (.*(?:\n .*)*)',blk,re.M): f[m.group(1)]=m.group(2)
    if 'Package' in f:
        pk.setdefault(f['Package'],f)
        for p in f.get('Provides','').split(','):
            p=p.strip().split(' ')[0]
            if p: prov.setdefault(p,f['Package'])
inst=set(subprocess.run(['dpkg-query','-W','-f=${Package}\n'],capture_output=True,text=True).stdout.split())
need=list(want); seen=set(); files=[]
while need:
    n=need.pop()
    if n in seen: continue
    seen.add(n)
    if n in inst and n not in want: continue
    f=pk.get(n) or pk.get(prov.get(n,''))
    if not f: print('missing',n); continue
    files.append(f['Filename'])
    for dep in f.get('Depends','').split(',')+f.get('Pre-Depends','').split(','):
        dep=dep.strip()
        if dep: need.append(dep.split('|')[0].strip().split(' ')[0].split(':')[0])
def get(fn):
    out=dest+'/debs/'+fn.split('/')[-1]
    if not os.path.exists(out): urllib.request.urlretrieve('https://deb.debian.org/debian/'+fn,out)
    return out
with cf.ThreadPoolExecutor(12) as ex: debs=list(ex.map(get,files))
for d in debs: subprocess.run(['dpkg-deb','-x',d,dest+'/root'],check=True)
print(len(debs),'packages ->',dest+'/root')
