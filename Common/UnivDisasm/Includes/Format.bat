@echo off
for /r %%i in (*.inc) do (
echo %%i
Formatter.exe -delphi %%i
)
pause