import collections,json,sys
O,PH=sys.argv[1],int(sys.argv[2]); TH=float(sys.argv[3]) if len(sys.argv)>3 else 4.0; MAXD=int(sys.argv[4]) if len(sys.argv)>4 else 30
j=json.load(open(O+'/idx.json')); idx={int(k):v.replace(' [chunk]','') for k,v in j['idx'].items()}
def nm(i): return idx.get(i,'?') if i>=0 else '<res/native>'
incl=collections.Counter(); selfc=collections.Counter(); n=0
tree={}
for l in open(O+'/samples.txt'):
    q=l.split()
    if len(q)<4 or int(q[0])!=PH: continue
    n+=1
    st=[nm(int(x)) for x in q[4:]][::-1]+[nm(int(q[3]))]
    # collapse direct recursion repeats
    path=[]
    for f in st:
        if not path or path[-1]!=f: path.append(f)
    for f in set(path): incl[f]+=1
    selfc[path[-1]]+=1
    t=tree
    for f in path:
        t=t.setdefault(f,[0,{}]); t[0]+=1; t=t[1]
print('samples',n)
print('--- inclusive')
for f,c in incl.most_common(45): print('%5.1f%%  self %4.1f%%  %s'%(100*c/n,100*selfc[f]/n,f))
print('--- tree (>= %.1f%%)'%TH)
def walk(t,d):
    for f,(c,ch) in sorted(t.items(),key=lambda x:-x[1][0]):
        if 100*c/n>=TH and d<MAXD:
            print('%s%4.1f%% %s'%('  '*d,100*c/n,f)); walk(ch,d+1)
walk(tree,0)
