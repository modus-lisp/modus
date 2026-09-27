#!/usr/bin/env python3
"""Stand-in for the PSP: decrypt a guest MSG_REPORT_REQ page, check it the way
the firmware would, and answer with a MSG_REPORT_RSP page carrying a synthetic
ATTESTATION_REPORT whose report_data echoes the request.  Also the reference
for the framing (built from the kernel's sev-guest.c conventions).

  fake-psp.py respond <vmpck.hex> <req.bin> <resp.bin> [measurement.hex]
  fake-psp.py check   <vmpck.hex> <req.bin>              # print decoded fields
"""
import sys, struct
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
HDR=96; AAD_OFF=0x30; AAD_LEN=48; PAGE=4096; REPORT_SIZE=1184
def open_msg(vmpck, page):
    seqno,=struct.unpack_from('<Q',page,0x20); algo,hver=page[0x30],page[0x31]
    hsz,=struct.unpack_from('<H',page,0x32); mtype,mver=page[0x34],page[0x35]
    msz,=struct.unpack_from('<H',page,0x36); vid=page[0x3C]
    assert algo==1 and hver==1 and hsz==96 and mver==1 and vid==0, (algo,hver,hsz,mver,vid)
    assert seqno!=0
    iv=struct.pack('<Q',seqno)+b'\0'*4
    pt=AESGCM(vmpck).decrypt(iv, bytes(page[HDR:HDR+msz])+bytes(page[0:16]), bytes(page[AAD_OFF:AAD_OFF+AAD_LEN]))
    return dict(seqno=seqno,type=mtype,payload=pt)
def build_msg(vmpck, seqno, mtype, payload):
    page=bytearray(PAGE)
    struct.pack_into('<Q',page,0x20,seqno); page[0x30]=1; page[0x31]=1
    struct.pack_into('<H',page,0x32,96); page[0x34]=mtype; page[0x35]=1
    struct.pack_into('<H',page,0x36,len(payload)); page[0x3C]=0
    iv=struct.pack('<Q',seqno)+b'\0'*4
    ct=AESGCM(vmpck).encrypt(iv, payload, bytes(page[AAD_OFF:AAD_OFF+AAD_LEN]))
    page[HDR:HDR+len(payload)]=ct[:-16]; page[0:16]=ct[-16:]
    return bytes(page)
def synthetic_report(report_data, vmpl, measurement):
    r=bytearray(REPORT_SIZE)
    struct.pack_into('<I',r,0,2); struct.pack_into('<I',r,4,7); struct.pack_into('<Q',r,8,0x30000)
    struct.pack_into('<I',r,0x30,vmpl); r[0x50:0x90]=report_data; r[0x90:0xC0]=measurement
    r[0x1A0:0x1E0]=bytes(range(64)); r[0x2A0:0x2A0+144]=b'\x5a'*144
    return bytes(r)
if __name__=='__main__':
    cmd=sys.argv[1]; vmpck=bytes.fromhex(open(sys.argv[2]).read().strip()); req=open(sys.argv[3],'rb').read()
    m=open_msg(vmpck, req)
    if cmd=='check':
        print('seqno',m['seqno'],'type',m['type'],'payload',m['payload'].hex()); sys.exit(0)
    assert m['type']==5, m['type']
    rd=m['payload'][:64]; vmpl,=struct.unpack_from('<I',m['payload'],64)
    meas=bytes.fromhex(sys.argv[5]) if len(sys.argv)>5 else bytes(range(48))
    resp=struct.pack('<II',0,REPORT_SIZE)+b'\0'*24+synthetic_report(rd,vmpl,meas)
    open(sys.argv[4],'wb').write(build_msg(vmpck, m['seqno']+1, 6, resp)); print('responded seqno',m['seqno']+1)
