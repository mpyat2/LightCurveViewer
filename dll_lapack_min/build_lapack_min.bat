rem Compile it inside the Intel oneAPI command prompt

SET PATH_TO_MKL=C:\Program Files (x86)\Intel\oneAPI\mkl\latest\

cl.exe /LD /MT /I"%PATH_TO_MKL%\include" lapack_min.c /link /LIBPATH:"%PATH_TO_MKL%\lib" mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib
editbin /RELEASE lapack_min.dll
