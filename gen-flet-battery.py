#!/usr/bin/env python3
# Extract each (deftest NAME BODY EXPECTED...) from an ANSI .lsp file and
# emit a Modus --load script: eval BODY, print name + fallback-count delta.
import sys, re

def read_forms(text):
    # crude s-expr reader that returns top-level forms as strings
    forms=[]; depth=0; start=None; instr=False; esc=False; i=0
    while i < len(text):
        c=text[i]
        if instr:
            if esc: esc=False
            elif c=='\\': esc=True
            elif c=='"': instr=False
        else:
            if c==';':
                # skip to EOL
                j=text.find('\n',i); i=(j if j>=0 else len(text)); continue
            elif c=='"': instr=True
            elif c=='(':
                if depth==0: start=i
                depth+=1
            elif c==')':
                depth-=1
                if depth==0:
                    forms.append(text[start:i+1])
        i+=1
    return forms

def split_toplevel(s):
    # s is "(deftest name body expected...)" -> list of subforms
    assert s[0]=='('
    inner=s[1:-1]
    subs=[]; depth=0; start=0; instr=False; esc=False; i=0; began=False
    while i<len(inner):
        c=inner[i]
        if instr:
            if esc: esc=False
            elif c=='\\': esc=True
            elif c=='"': instr=False
            i+=1; continue
        if c==';':
            j=inner.find('\n',i); i=(j if j>=0 else len(inner)); continue
        if c in ' \t\n':
            if began and depth==0:
                subs.append(inner[start:i]); began=False
            i+=1; continue
        if not began:
            began=True; start=i
        if c=='"': instr=True
        elif c=='(': depth+=1
        elif c==')': depth-=1
        i+=1
    if began: subs.append(inner[start:])
    return subs

fn=sys.argv[1]
text=open(fn).read()
print(';; auto-generated flet/labels battery')
print('(defvar *fb0* 0)')
for form in read_forms(text):
    subs=split_toplevel(form)
    if not subs or subs[0].lower()!='deftest': continue
    name=subs[1]
    body=subs[2] if len(subs)>2 else 'nil'
    # escape body inside a string not needed; embed directly
    body_1line=body.replace('\n',' ')
    print(f'(setq *fb0* (or *jit-fallback-count* 0))')
    print(f'(handler-case (let ((v (progn {body_1line})))')
    print(f'  (format t "~A val=~S fbdelta=~A terr=~A~%" "{name}" v (- (or *jit-fallback-count* 0) *fb0*) *jit-translate-err-count*))')
    print(f'  (t (c) (format t "~A CRASH ~A terr=~A~%" "{name}" c *jit-translate-err-count*)))')
