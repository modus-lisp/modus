import collections,json,sys
O,PH,F=sys.argv[1],int(sys.argv[2]),sys.argv[3]; D=int(sys.argv[4]) if len(sys.argv)>4 else 3
j=json.load(open(O+'/idx.json')); idx={int(k):v.replace(' [chunk]','') for k,v in j['idx'].items()}
def nm(i): return idx.get(i,'?') if i>=0 else '<res>'
c=collections.Counter(); n=0; tot=0
for l in open(O+'/samples.txt'):
    q=l.split()
    if len(q)<4 or int(q[0])!=PH: continue
    n+=1
    st=[nm(int(x)) for x in q[4:]][::-1]+[nm(int(q[3]))]
    path=[]
    for f in st:
        if not path or path[-1]!=f: path.append(f)
    if F in path:
        i=path.index(F); tot+=1
        c[' < '.join(path[max(0,i-D):i][::-1])]+=1
print(F,'%.1f%%'%(100*tot/n))
for k,v in c.most_common(12): print('  %4.1f%%  %s'%(100*v/n,k))
