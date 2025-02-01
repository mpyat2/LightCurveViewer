@echo off
set lazbuild_cmd=C:\Personal\lazarus64\lazbuild.exe
%lazbuild_cmd% LCV.lpr --build-mode=NoDebug 2>&1 >LCV-build.log
echo See LCV-build.log
