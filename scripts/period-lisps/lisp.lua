-- lisp.lua: MAME autoboot driver.  Wait DELAY emulated seconds, then type each form of
-- MAC_FORMS (separated by |) followed by Return, GAP emulated seconds apart; a screenshot
-- SNAPDIR/shot-NN.png before each form and one at the end, then exit.
local forms = {}
for f in string.gmatch(os.getenv("MAC_FORMS") or "", "([^|]+)") do table.insert(forms, f) end
local delay, gap = tonumber(os.getenv("DELAY") or "60"), tonumber(os.getenv("GAP") or "30")
local snapdir = os.getenv("SNAPDIR")
local i, nextt = 1, delay
emu.register_frame_done(function()
  local t = manager.machine.time.seconds
  if t >= nextt then
    manager.machine.screens:at(1):snapshot(string.format("%s/shot-%02d.png", snapdir, i))
    print(string.format("lisp.lua: shot-%02d at emulated %.3f s", i, t))
    if i <= #forms then
      -- NOTYPE=1: screenshots only (the forms just count them)
      if not os.getenv("NOTYPE") then manager.machine.natkeyboard:post(forms[i] .. "\r") end
      i = i + 1; nextt = t + gap
    else
      manager.machine:exit()
    end
  end
end)

-- PCLOG=T: from emulated second T, every 20 ms, PC and registers to SNAPDIR/pclog.txt
-- (where a launched application goes wrong).  PCLOG_END stops it (default T + 40).
if os.getenv("PCLOG") then
  local t0 = tonumber(os.getenv("PCLOG")); local t1 = tonumber(os.getenv("PCLOG_END") or (t0 + 40))
  local cpu = manager.machine.devices[":maincpu"]
  local f = io.open(snapdir .. "/pclog.txt", "w"); local last = -1
  emu.register_periodic(function()
    local t = manager.machine.time:as_double()
    if t < t0 or t > t1 or math.floor(t * 50) == last then return end
    last = math.floor(t * 50)
    local s = cpu.state
    f:write(string.format("%.2f PC=%X SR=%X A7=%X A6=%X A5=%X A4=%X A3=%X A2=%X A1=%X A0=%X D0=%X D1=%X\n", t,
      s["PC"].value, s["SR"].value, s["A7"].value, s["A6"].value, s["A5"].value, s["A4"].value,
      s["A3"].value, s["A2"].value, s["A1"].value, s["A0"].value, s["D0"].value, s["D1"].value))
    f:flush()
  end)
end

-- WTAP=LO:HI:PCLO:PCHI (hex): log writes to [LO, HI) made by code at [PCLO, PCHI) to
-- SNAPDIR/wtap.txt, each (PC, address) once (who writes where it should not)
if os.getenv("WTAP") then
  local a = {}
  for x in string.gmatch(os.getenv("WTAP"), "([^:]+)") do table.insert(a, tonumber(x, 16)) end
  local cpu = manager.machine.devices[":maincpu"]
  local mem = cpu.spaces["program"]
  local f = io.open(snapdir .. "/wtap.txt", "w"); local seen = {}; local n = 0
  wtap = mem:install_write_tap(a[1], a[2] - 1, "wtap", function(offset, data, mask)
    local pc = cpu.state["PC"].value
    if pc >= a[3] and pc < a[4] and n < 400 then
      local k = string.format("%X %X", pc, offset)
      if not seen[k] then
        seen[k] = true; n = n + 1
        f:write(string.format("%.3f PC=%X addr=%X data=%X\n", manager.machine.time:as_double(), pc, offset, data)); f:flush()
      end
    end
  end)
end

-- TRACEAT=ADDR (hex), with MAMEARGS="-debug -debugger none": an instruction trace of
-- the CPU from the first time it reaches ADDR, TRACEN instructions, to SNAPDIR/trace.txt
if os.getenv("TRACEAT") and manager.machine.debugger then
  local dbg = manager.machine.debugger
  dbg:command(string.format("bpset %s,1,{trace %s/trace.txt,maincpu,noloop;g}", os.getenv("TRACEAT"), snapdir))
  dbg:command("go")
end

-- PBTAP=OFFSET (hex): at the first screenshot, BASE = ApplLimit ($130, where
-- mac68k-chain.s's relocating load leaves it); log every write to the 80 bytes
-- at BASE + OFFSET (the image's _Read parameter block) to SNAPDIR/pbtap.txt
if os.getenv("PBTAP") then
  local cpu = manager.machine.devices[":maincpu"]
  local mem = cpu.spaces["program"]
  local f = io.open(snapdir .. "/pbtap.txt", "w"); local armed = false
  emu.register_frame_done(function()
    if armed or manager.machine.time:as_double() < delay then return end
    armed = true
    local base = mem:read_u32(0x130)
    local pb = base + tonumber(os.getenv("PBTAP"), 16)
    f:write(string.format("BASE=%X pb=%X\n", base, pb)); f:flush()
    pbtap = mem:install_write_tap(pb, pb + 79, "pbtap", function(offset, data, mask)
      f:write(string.format("%.3f PC=%X +%02X=%X mask=%X\n", manager.machine.time:as_double(),
        cpu.state["PC"].value, offset - pb, data, mask)); f:flush()
    end)
  end)
end

-- CMDKEY=T:C: at emulated second T, hold Command and type C (a menu's key
-- equivalent: CMDKEY=200:q quits an application), then let Command go
if os.getenv("CMDKEY") then
  local t, c = string.match(os.getenv("CMDKEY"), "([^:]+):(.+)")
  t = tonumber(t)
  local cmd
  for _, port in pairs(manager.machine.ioport.ports) do
    for name, field in pairs(port.fields) do
      if string.find(string.lower(name), "command") then cmd = field end
    end
  end
  local stage = 0
  emu.register_periodic(function()
    local now = manager.machine.time:as_double()
    if stage == 0 and now >= t then
      if cmd then cmd:set_value(1) end; stage = 1
    elseif stage == 1 and now >= t + 0.3 then
      manager.machine.natkeyboard:post(c); stage = 2
    elseif stage == 2 and now >= t + 1.5 then
      if cmd then cmd:clear_value() end; stage = 3
      print(string.format("lisp.lua: Command-%s at %.1f s (key %s)", c, t, cmd and cmd.name or "NOT FOUND"))
    end
  end)
end
