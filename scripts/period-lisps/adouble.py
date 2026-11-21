import struct
def parse(blob):
    """AppleDouble -> dict(rsrc=bytes, type=b'', creator=b'', flags=int)."""
    magic, ver = struct.unpack_from('>II', blob, 0)
    assert magic == 0x00051607, hex(magic)
    n = struct.unpack_from('>H', blob, 24)[0]
    out = {'rsrc': b'', 'type': b'????', 'creator': b'????', 'flags': 0}
    for i in range(n):
        eid, off, ln = struct.unpack_from('>III', blob, 26 + 12 * i)
        if eid == 2: out['rsrc'] = blob[off:off + ln]
        if eid == 9: out['type'], out['creator'] = blob[off:off + 4], blob[off + 4:off + 8]; out['flags'] = struct.unpack_from('>H', blob, off + 8)[0]
    return out
