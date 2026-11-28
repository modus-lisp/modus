@echo off
rem desk.cmd -- the glass desk (Windows): a window manager served to a web browser and
rem over VNC.  Double-click it, or run it in a console with flags: desk --help.
rem The console stays open after an error, so a double-click shows the message.
setlocal
cd /d "%~dp0"
modus-windows-x64.exe --dynamic-space-size 8192 --script src/modus/scripts/glass-desk.lisp %*
set rc=%errorlevel%
if not "%rc%"=="0" (
  echo.
  echo desk stopped with exit code %rc%.
  pause
)
exit /b %rc%
