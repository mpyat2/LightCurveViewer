@echo off

del out\lcv*.exe
rem del setup\lcvsetup*.exe

set setupfilename=lcvsetup64
set exename=lcv64
set lazbuild_cmd=C:\lazarus64\lazbuild.exe
set ISCC="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"

%lazbuild_cmd% lcv.lpr --build-mode=NoDebug 2>&1 >lcv-build.log
if errorlevel 1 goto :ERROR 

rename out\lcv.exe %exename%.exe

cd setup
%ISCC% lcvsetup.iss /F%setupfilename% /D%exename%

echo See lcv-build.log

goto :END

:ERROR
echo ***** ERROR! CANNOT COMPILE.

:END
rem pause
