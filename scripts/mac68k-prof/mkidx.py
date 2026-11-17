import struct,json,sys,os
D,O=sys.argv[1],sys.argv[2]; LOAD=int(sys.argv[3],0)
img=open(D+'/cl.bin','rb').read()
names={}
for l in open(D+'/cl.map'):
    if l.startswith('#'): continue
    p=l.rstrip('\n').split('\t')
    if len(p)<4: continue
    a=int(p[0],16)-LOAD
    if 0<=a<len(img)-8 and img[a:a+4]==b'\x22\x4d\x30\x3c':
        i,=struct.unpack_from('>H',img,a+4); names.setdefault(i,p[3])
k=img.find(b'modusPGD'); table,cnt,sec=struct.unpack_from('>III',img,k+8)
out={}; cur='?'
for i in range(cnt):
    if i in names: cur=names[i]; out[i]=cur
    else: out[i]=cur+' [chunk]'
os.makedirs(O,exist_ok=True)
json.dump({'idx':out,'table':table,'cnt':cnt},open(O+'/idx.json','w'))
print('table %X cnt %d'%(table,cnt))
