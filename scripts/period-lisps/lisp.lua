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
      -- a form "~": nothing typed (a screenshot only)
      if not os.getenv("NOTYPE") and forms[i] ~= "~" then manager.machine.natkeyboard:post(forms[i] .. "\r") end
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
  local hz = tonumber(os.getenv("PCLOG_HZ") or "50")   -- samples a second (at most a frame's worth)
  local mem = cpu.spaces["program"]; local based = false
  emu.register_periodic(function()
    local t = manager.machine.time:as_double()
    if t < t0 or t > t1 or math.floor(t * hz) == last then return end
    last = math.floor(t * hz)
    -- ApplLimit ($130): where mac68k-chain.s's relocating load put the map
    if not based then f:write(string.format("BASE %X\n", mem:read_u32(0x130))); based = true end
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

-- MOUSE="T:V:H:B;...": at emulated second T, the cursor to (V, H) -- written
-- where the Mac keeps it (MTemp $828, RawMouse $82C, then CrsrNew $8CE =
-- CrsrCouple, as the mouse driver does) -- and the button down (B=1) or up
-- (B=0), through the emulated mouse's button.  A drag is several steps.
if os.getenv("MOUSE") then
  local steps = {}
  for t, v, h, b in string.gmatch(os.getenv("MOUSE"), "([%d%.]+):(%d+):(%d+):(%d)") do
    table.insert(steps, {tonumber(t), tonumber(v), tonumber(h), tonumber(b)})
  end
  local cpu = manager.machine.devices[":maincpu"]
  local mem = cpu.spaces["program"]
  local button
  for tag, port in pairs(manager.machine.ioport.ports) do
    for name, field in pairs(port.fields) do
      local n = string.lower(name)
      -- the Quadra's names it; an ADB machine's (LC III: :macadb:MOUSE0) is
      -- the default "P1 Button 1"
      if n == "mouse button 0" then button = field end
      if not button and string.match(tag, "MOUSE0$") and string.find(n, "button") then button = field end
    end
  end
  print("lisp.lua: mouse button " .. (button and button.name or "NOT FOUND"))
  local i = 1
  emu.register_periodic(function()
    local now = manager.machine.time:as_double()
    while i <= #steps and now >= steps[i][1] do
      local s = steps[i]
      mem:write_u16(0x828, s[2]); mem:write_u16(0x82A, s[3])
      mem:write_u16(0x82C, s[2]); mem:write_u16(0x82E, s[3])
      mem:write_u8(0x8CE, mem:read_u8(0x8CF))
      if button then
        if s[4] == 1 then button:set_value(1) else button:clear_value() end
      end
      i = i + 1
    end
  end)
end

-- PROF=T0:T1: from emulated second T0 to T1, every PROF_US microseconds
-- (default 1000), the PC and the return addresses up the A6 frame chain
-- (LINK frames: [A6] the caller's A6, [A6+4] its return address), to
-- SNAPDIR/prof.txt as "T PC RET1 RET2 ...".  The first line is "BASE b":
-- ApplLimit ($130), where mac68k-chain.s's relocating load put the map
-- (scripts/mac68k-prof/native.lisp maps the addresses through cl.map).
-- Sampled by a coroutine sleeping on emu.wait (emulated time), NOT a frame
-- callback: those run at the video frame's end, exactly when the Mac's VBL
-- interrupt does, and saw almost nothing but its handler.
if os.getenv("PROF") then
  local t0, t1 = string.match(os.getenv("PROF"), "([%d%.]+):([%d%.]+)")
  t0, t1 = tonumber(t0), tonumber(t1)
  local step = tonumber(os.getenv("PROF_US") or "1000") / 1e6
  local cpu = manager.machine.devices[":maincpu"]
  local mem = cpu.spaces["program"]
  local f = io.open(snapdir .. "/prof.txt", "w")
  local function sample(t)
    local s = cpu.state
    local out = { string.format("%.4f %X SP=%X SR=%X", t, s["PC"].value, s["A7"].value, s["SR"].value) }
    if os.getenv("PROF_REGS") then
      for _, r in ipairs({"A0","A1","A2","A3","A4","A5","A6","D0","D1","D2","D3"}) do
        table.insert(out, string.format("%s=%X", r, s[r].value))
      end
      table.insert(out, "|")
    end
    local a6 = s["A6"].value
    for i = 1, 48 do
      if a6 == 0 or a6 % 2 == 1 or a6 > 0x2000000 then break end
      local nxt, ret = mem:read_u32(a6), mem:read_u32(a6 + 4)
      table.insert(out, string.format("%X", ret))
      if nxt == a6 then break end
      a6 = nxt
    end
    f:write(table.concat(out, " ") .. "\n")
  end
  local co = coroutine.create(function()
    emu.wait(t0)
    f:write(string.format("BASE %X\n", mem:read_u32(0x130)))
    while manager.machine.time:as_double() < t1 do
      sample(manager.machine.time:as_double())
      emu.wait(step)
    end
    f:close()
  end)
  local ok, err = coroutine.resume(co)
  if not ok then print("lisp.lua: PROF: " .. tostring(err)) end
end

-- MEMDUMP="T:ADDR:LEN,...": at emulated second T, LEN bytes from ADDR (hex) to
-- SNAPDIR/mem-ADDR.bin (code to disassemble where nothing has symbols)
if os.getenv("MEMDUMP") then
  local mem = manager.machine.devices[":maincpu"].spaces["program"]
  for t, a, n in string.gmatch(os.getenv("MEMDUMP"), "([%d%.]+):(%x+):(%x+)") do
    local co = coroutine.create(function()
      emu.wait(tonumber(t))
      local f = io.open(string.format("%s/mem-%s.bin", snapdir, a), "wb")
      local base, len = tonumber(a, 16), tonumber(n, 16)
      for i = 0, len - 1 do f:write(string.char(mem:read_u8(base + i))) end
      f:close()
    end)
    coroutine.resume(co)
  end
end

-- FLOPLOAD="T:PATH;...": at emulated second T, insert the floppy image PATH into
-- the first floppy drive (a disk swap: modus's two-disk sets eject disk 1 and
-- ask for disk 2, scripts/mac68k-disk2.s)
if os.getenv("FLOPLOAD") then
  for t, path in string.gmatch(os.getenv("FLOPLOAD"), "([%d%.]+):([^;]+)") do
    local co = coroutine.create(function()
      emu.wait(tonumber(t))
      for tag, img in pairs(manager.machine.images) do
        print(string.format("lisp.lua: image %s (%s / %s)", tag, tostring(img.instance_name),
                            tostring(img.brief_instance_name)))
      end
      for tag, img in pairs(manager.machine.images) do
        local b = tostring(img.brief_instance_name)
        if b == "flop1" or b == "flop" then
          local err = img:load(path)
          print(string.format("lisp.lua: %s into %s at %.1f s: %s", path, tag,
                              manager.machine.time:as_double(), tostring(err or "ok")))
          break
        end
      end
    end)
    coroutine.resume(co)
  end
end
