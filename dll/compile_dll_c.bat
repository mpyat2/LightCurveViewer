rem gcc -shared -s -o sincos.dll sincos.c
cl.exe /LD /MT sincos.c
editbin /RELEASE sincos.dll
