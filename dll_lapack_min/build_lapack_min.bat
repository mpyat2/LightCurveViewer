rem Compile it inside the Intel oneAPI command prompt
rem This is a thread-save variant
cl /LD /MT lapack_min.c dllmain.c ^
   mkl_intel_lp64.lib mkl_core.lib mkl_sequential.lib ^
   /link /OPT:REF /OPT:ICF /INCREMENTAL:NO /DEBUG:NONE
editbin /RELEASE dgels_min.dll


