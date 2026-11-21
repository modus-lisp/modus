-- mac68k-mame-serial.lua -- MAME autoboot script for scripts/mac68k-ladder.py on
-- a Mac Classic (macclasc): the bytes the Mac sends on serial port A, written
-- to $MAC68K_SERIAL_OUT as they go.  MAME 0.251's Classic has no RS-232 port,
-- so the oracle is a write tap on the SCC: on the Classic the CPU writes the
-- SCC at #xBFFFF9.. (SCCWr), channel A data at #xBFFFFF -- an odd-byte write
-- MAME delivers as offset #xBFFFFE, mask #x00FF.  Measured: rung r01's four
-- bytes, 2A 00 00 00.  Exits after $MAC68K_SERIAL_N bytes, or at
-- $MAC68K_SECONDS of emulated time.
local mem = manager.machine.devices[":maincpu"].spaces["program"]
local out = io.open(os.getenv("MAC68K_SERIAL_OUT"), "wb")
local want = tonumber(os.getenv("MAC68K_SERIAL_N") or "0")
local limit = tonumber(os.getenv("MAC68K_SECONDS") or "120")
local n = 0
tap = mem:install_write_tap(0xBFFFFE, 0xBFFFFF, "serial-a", function(offset, data, mask)
  if offset == 0xBFFFFE and (mask & 0xFF) ~= 0 then
    out:write(string.char(data & 0xFF)); out:flush()
    n = n + 1
  end
end)
emu.register_frame_done(function()
  if (want > 0 and n >= want) or manager.machine.time.seconds >= limit then
    out:close()
    manager.machine:exit()
  end
end)
