-- mac68k-mame-screen.lua -- drive the modus REPL on MAME by its SCREEN console
-- (no serial port in MAME's SE/30 / II / Quadra; the Classic works the same way).
--
-- The prompt is read from the console's text buffer in the image's runtime-data
-- region (mac-console.lisp: cursor x/y, cols, rows, top row, buffer).  Each form is
-- typed, and when the echo has been still for a second Return is pressed: the clock
-- starts THERE, so the times are evaluation, like MCL's TIME, not typing speed.
--
--   MAC_FORMS    forms separated by |              MAC_OUT     transcript file
--   MAC_SECONDS  emulated-time limit (600)         MAC_SNAP    screenshot at the end
--   REGION       the runtime-data region (#x2B0000 on classicos)
--   FAKESCREEN   ScrnBase to poke at boot (the Quadra's visible VRAM gives SysError 51).
--                Use RAM the image never touches: 0x600000 on an 8 MB Quadra.  NOT
--                0xF9101000: the console then draws over RAM from 0x101000 and breaks
--                any image whose static heap reaches past 1 MB.
--   MAC_SAMPLES  + TBL, CNT (scripts' mkidx.py): a call-stack sampler for each form,
--                from Return to the prompt: "PHASE PC A5 OWNER(A5) OWNER(RET)..."
--   MAC_LOADS    with MAC_SAMPLES: "PHASE STREAM" per stream the pager loads
--   PCLOG        T: log PC and registers every 50 ms from emulated second T
local cpu = manager.machine.devices[":maincpu"]
local mem = cpu.spaces["program"]
local R = tonumber(os.getenv("REGION") or "0x2B0000")
local out = io.open(os.getenv("MAC_OUT"), "w")
local forms, i = {}, 1
for f in string.gmatch(os.getenv("MAC_FORMS") or "", "([^|]+)") do table.insert(forms, f) end
local limit = tonumber(os.getenv("MAC_SECONDS") or "600")
local state, t0, phase = "boot", 0, 0
local tq, qx, qy = 0, -1, -1
local lastpcl = -1

local function u32(a) return mem:read_u32(R + a) end
local function line(y)
  local cols, rows, top = u32(0x9208), u32(0x920C), u32(0x9224)
  if cols == 0 or rows == 0 or cols > 2000 or rows > 200 then return "" end
  local r = (top + y) % rows
  local s = {}
  for x = 0, cols - 1 do
    local c = mem:read_u8(R + 0xA000 + r * cols + x)
    if c < 32 or c > 126 then c = 32 end
    s[#s + 1] = string.char(c)
  end
  return (table.concat(s):gsub("%s+$", ""))
end
local function at_prompt()
  local x, y = u32(0x9200), u32(0x9204)
  return x == 2 and line(y) == ">"
end

-- the sampler (MAC_SAMPLES): the threaded IP's owner and the A6 frame chain's
local samples, loads
if os.getenv("MAC_SAMPLES") then
  samples = io.open(os.getenv("MAC_SAMPLES"), "w")
  local TBL, CNT = tonumber(os.getenv("TBL")), tonumber(os.getenv("CNT"))
  if os.getenv("MAC_LOADS") and TBL then
    loads = io.open(os.getenv("MAC_LOADS"), "w")
    ltap = mem:install_write_tap(TBL, TBL + 4 * CNT - 1, "streams", function(offset, data, mask)
      if (offset & 3) == 0 and (data & 1) == 0 and phase > 0 then
        loads:write(string.format("%d %d\n", phase, (offset - TBL) // 4))
      end
    end)
  end
  emu.register_periodic(function()
    if phase == 0 then return end
    local map = mem:read_u32(R + 0x6040); local lo = mem:read_u32(R + 0x603C); local hi = mem:read_u32(R + 0x6038)
    local function owner(a)
      if a >= lo and a < hi then return mem:read_u16(map + ((a - map) >> 6) * 2) - 1 end
      return -1
    end
    local a5 = cpu.state["A5"].value
    local t = {string.format("%d %X %X %d", phase, cpu.state["PC"].value, a5, owner(a5))}
    local fp = cpu.state["A6"].value; local base = mem:read_u32(R + 0x58); local n = 0
    while fp >= 0x8000 and fp < base and n < 400 do
      t[#t + 1] = tostring(owner(mem:read_u32(fp - 4)))
      local nx = mem:read_u32(fp); if nx <= fp then break end
      fp = nx; n = n + 1
    end
    samples:write(table.concat(t, " ") .. "\n")
  end)
end

emu.register_frame_done(function()
  local t = manager.machine.time:as_double()
  if os.getenv("FAKESCREEN") and t > 8.5 and t < 11 then mem:write_u32(0x824, tonumber(os.getenv("FAKESCREEN"))) end
  if os.getenv("PCLOG") and t > tonumber(os.getenv("PCLOG")) and math.floor(t * 20) ~= lastpcl then
    lastpcl = math.floor(t * 20)
    out:write(string.format("[pc %.2f PC=%X SR=%X A7=%X A5=%X D0=%X A0=%X A1=%X]\n", t, cpu.state["PC"].value,
      cpu.state["SR"].value, cpu.state["A7"].value, cpu.state["A5"].value, cpu.state["D0"].value,
      cpu.state["A0"].value, cpu.state["A1"].value))
  end
  if state == "boot" or state == "busy" then
    if at_prompt() then
      if state == "busy" then
        local y = u32(0x9204)
        out:write(string.format("[%.1f s: done %.1f s] %s\n", t, t - t0, line(y - 1))); phase = 0
      else
        out:write(string.format("[%.1f s: prompt]\n", t))
      end
      out:flush()
      if i <= #forms then
        out:write(string.format("[%.1f s: typing %s]\n", t, forms[i])); out:flush()
        manager.machine.natkeyboard:post(forms[i])
        i = i + 1; state = "typing"; t0 = t; tq = t; qx, qy = -1, -1
      else
        state = "end"
      end
    end
  elseif state == "typing" then
    local x, y = u32(0x9200), u32(0x9204)
    if x ~= qx or y ~= qy then qx, qy, tq = x, y, t end
    local kb = manager.machine.natkeyboard
    local empty = true
    pcall(function() empty = kb.empty end)
    if at_prompt() and t - t0 > 8 then
      -- nothing echoed: the keyboard was not listening yet; type it again
      out:write(string.format("[%.1f s: retyping]\n", t)); out:flush()
      kb:post(forms[i - 1]); t0 = t
    elseif not at_prompt() and empty and t - tq > 1.0 then
      out:write(string.format("[%.1f s: typed in %.1f s, Return]\n", t, t - t0)); out:flush()
      kb:post("\r"); state = "busy"; t0 = t; phase = i - 1
    end
  end
  if t >= limit or state == "end" then
    local w = {}
    for k = 0, 0x28, 4 do w[#w + 1] = string.format("%X", u32(0x9200 + k)) end
    out:write("[console words 9200..: " .. table.concat(w, " ") .. string.format("] ScrnBase=%X\n", mem:read_u32(0x824)))
    out:write(string.format("[stopped at %.1f s emulated]\n", t)); out:close()
    if samples then samples:close() end
    if loads then loads:close() end
    manager.machine.screens:at(1):snapshot(os.getenv("MAC_SNAP") or "end.png")
    manager.machine:exit()
  end
end)
