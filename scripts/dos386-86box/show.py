import sys,json
for l in sys.stdin:
    try: r=json.loads(l)
    except: print(l[:300]); continue
    if 'screen' in r: print('\n'.join(x for x in r['screen'] if x.strip()))
    elif 'text' in r: print('COM1:', repr(r['text'][-800:]))
    elif not r.get('ok'): print(r)
