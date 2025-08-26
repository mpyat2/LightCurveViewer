// file: dllmain.c
#include "mkl.h"
#include <windows.h>

BOOL APIENTRY DllMain(HMODULE hModule,
                      DWORD  ul_reason_for_call,
                      LPVOID lpReserved)
{
    if (ul_reason_for_call == DLL_PROCESS_ATTACH)
    {
        // These calls are needed for the multi-threaded DLL only.
        // Otherwise they are ignored
        mkl_set_dynamic(0);
        mkl_set_num_threads(1); // disable nested threading
    }
    return TRUE;
}
