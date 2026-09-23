import re,collections,glob,os,sys
log=sys.argv[1]
root='/home/claude/modus/tmp/ansi-test/'
src=open('/home/claude/modus/mvm/build-ansi-common.lisp').read()
listed=set(re.findall(r'"([a-z0-9-]+)\.lsp"',src))
s=collections.OrderedDict()
for ln in open('/home/claude/modus/tmp/ansi-census/sbcl-tests.txt'):
    f,n=ln.rstrip('\n').split(' ',1); s[n]=f
m={}; cur=None; transformed=set()
for ln in open(log,errors='replace'):
    mm=re.match(r'\s+Transforming: (\S+)\.lsp',ln)
    if mm: cur=mm.group(1); transformed.add(cur); continue
    mm=re.match(r'\s+(\d+) = (.+)$',ln)
    if mm and cur: m.setdefault(mm.group(2).strip(),cur)
paths={}
for p in glob.glob(root+'**/*.lsp',recursive=True):
    paths.setdefault(os.path.basename(p)[:-4],p)
texts={}
def txt(f):
    if f not in texts: texts[f]=open(paths[f],errors='replace').read().upper() if f in paths else ''
    return texts[f]
missing=[n for n in s if n not in m]
cat=collections.Counter(); bycat=collections.defaultdict(collections.Counter)
for n in missing:
    f=s[n]
    if f not in listed: c='file-not-listed'
    elif f not in transformed: c='listed-not-transformed'
    elif re.search(r'^\(DEFTEST\s+'+re.escape(n)+r'(\s|$)',txt(f),re.M): c='toplevel-deftest-dropped'
    elif re.search(r'\(DEFTEST\s+'+re.escape(n)+r'(\s|$)',txt(f)): c='nested-deftest'
    elif re.search(r'^\(DEF[A-Z0-9-]*\s+'+re.escape(n)+r'(\s|$)',txt(f),re.M): c='toplevel-macro-call'
    else: c='generated'
    cat[c]+=1; bycat[c][f]+=1
print("sbcl",len(s),"gate",len(m),"missing",len(missing),"gate-only",len([n for n in m if n not in s]))
for c,k in cat.most_common():
    print(f"{k:5d} {c}: "+", ".join(f"{f}({x})" for f,x in bycat[c].most_common(12)))
open('/home/claude/modus/tmp/ansi-census/missing.txt','w').write('\n'.join(f"{s[n]} {n}" for n in missing))
