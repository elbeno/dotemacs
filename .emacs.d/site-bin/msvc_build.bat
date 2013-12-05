@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86
devenv "%1" /Build %2
exit /b %errorlevel%
