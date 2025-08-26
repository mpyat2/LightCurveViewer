#include "mkl.h"

// Wrapper for DGELS
// We use __declspec(dllexport) to make it visible in the DLL
__declspec(dllexport)
void dgels_wrapper(const char *trans, const MKL_INT *m, const MKL_INT *n,
                   const MKL_INT *nrhs, double *a, const MKL_INT *lda,
                   double *b, const MKL_INT *ldb,
                   double *work, const MKL_INT *lwork, MKL_INT *info)
{
    dgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info);
}
