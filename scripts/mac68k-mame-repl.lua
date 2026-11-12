-- drive the modus REPL on MAME's Mac Classic: serial port A out (SCC tap),
-- forms typed on the keyboard when the prompt shows, a screenshot at the end
local mem = manager.machine.devices[":maincpu"].spaces["program"]
local out = io.open(os.getenv("MAC_OUT"), "w")
local forms, i = {}, 1
for f in string.gmatch(os.getenv("MAC_FORMS") or "", "([^|]+)") do table.insert(forms, f) end
local limit = tonumber(os.getenv("MAC_SECONDS") or "600")
local text, typed_at = "", -1
tap = mem:install_write_tap(0xBFFFFE, 0xBFFFFF, "serial-a", function(offset, data, mask)
  if offset == 0xBFFFFE and (mask & 0xFF) ~= 0 then
    local c = string.char(data & 0xFF)
    text = text .. c
    out:write(c); out:flush()
  end
end)
emu.register_frame_done(function()
  local t = manager.machine.time.seconds
  if i <= #forms and string.sub(text, -2) == "> " and #text ~= typed_at then
    typed_at = #text
    out:write(string.format("[%.1f s: typing %s]", t, forms[i])); out:flush()
    manager.machine.natkeyboard:post(forms[i] .. "\r")
    i = i + 1
  end
  if t >= limit or (i > #forms and string.sub(text, -2) == "> " and #text ~= typed_at) then
    out:write(string.format("\n[stopped at %.1f s emulated]\n", t)); out:close()
    manager.machine.screens:at(1):snapshot(os.getenv("MAC_SNAP") or "end.png")
    manager.machine:exit()
  end
end)
