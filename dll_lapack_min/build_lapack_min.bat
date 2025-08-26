rem Compile it inside the Intel oneAPI command prompt
rem This is a thread-safe variant
::cl /LD /MT lapack_min.c dllmain.c ^
::   mkl_intel_lp64.lib mkl_core.lib mkl_sequential.lib ^
::   /link /OPT:REF /OPT:ICF /INCREMENTAL:NO /DEBUG:NONE
::editbin /RELEASE dgels_min.dll

SET PATH_TO_MKL=C:\Program Files (x86)\Intel\oneAPI\mkl\latest\

cl.exe /LD /MT /I"%PATH_TO_MKL%\include" lapack_min.c dllmain.c /link /LIBPATH:"%PATH_TO_MKL%\lib" mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib
editbin /RELEASE lapack_min.dll
