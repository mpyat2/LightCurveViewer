// file lapack_min.c
#include "mkl.h"
#include <stdlib.h>

// Wrapper: solves least squares via DGELS in one call
// Arguments identical to DGELS except no 'work'/'lwork'
__declspec(dllexport)
void dgels_solve(const char *trans,
                 const MKL_INT *m, const MKL_INT *n, const MKL_INT *nrhs,
                 double *a, const MKL_INT *lda,
                 double *b, const MKL_INT *ldb,
                 MKL_INT *info)
{
    double wkopt;
    double *work;
    MKL_INT lwork;

    // Step 1: Workspace query
    lwork = -1;
    dgels(trans, m, n, nrhs, a, lda, b, ldb, &wkopt, &lwork, info);
    if (*info != 0) return;

    // Step 2: Allocate optimal workspace
    lwork = (MKL_INT)wkopt;
    work = (double*)malloc(lwork * sizeof(double));
    if (!work) {
        *info = -100; // custom error code for "out of memory"
        return;
    }

    // Step 3: Actual computation
    dgels(trans, m, n, nrhs, a, lda, b, ldb, work, &lwork, info);

    // Step 4: Cleanup
    free(work);
}
