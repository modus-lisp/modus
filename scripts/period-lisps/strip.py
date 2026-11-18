"""strip.py RUNDIR [Y0 Y1] -- stack the Listener band (rows Y0..Y1, default 215..330) of
RUNDIR/shot-02.png .. shot-NN.png into RUNDIR/strip.png, one image to read all results
from.  Needs Pillow (the venv has it: $LISPS/venv/bin/pip install pillow)."""
import glob, os, sys
from PIL import Image
d = sys.argv[1]
y0, y1 = (int(sys.argv[2]), int(sys.argv[3])) if len(sys.argv) > 3 else (215, 330)
shots = sorted(glob.glob(os.path.join(d, 'shot-*.png')))[1:]
ims = [Image.open(p).crop((0, y0, Image.open(p).width, y1)) for p in shots]
out = Image.new('RGB', (max(i.width for i in ims), sum(i.height + 2 for i in ims)), 'gray')
y = 0
for i in ims: out.paste(i, (0, y)); y += i.height + 2
out.save(os.path.join(d, 'strip.png'))
print(os.path.join(d, 'strip.png'), len(ims), 'shots')
