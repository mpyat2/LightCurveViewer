@echo off
set lazbuild_cmd=C:\Personal\lazarus64\lazbuild.exe
%lazbuild_cmd% lcv.lpr --build-mode=NoDebug 2>&1 >lcv-build.log
echo See lcv-build.log
