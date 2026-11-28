@echo off
rem repl.cmd -- the modus REPL (Windows).  Double-click it, or run it in a console; any
rem arguments go to modus.  The console stays open after an error.
setlocal
"%~dp0modus-windows-x64.exe" %*
set rc=%errorlevel%
if not "%rc%"=="0" (
  echo.
  echo modus stopped with exit code %rc%.
  pause
)
exit /b %rc%
