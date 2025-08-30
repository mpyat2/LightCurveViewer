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

// Invert an n x n matrix A (row-major) in place using Intel MKL LAPACKE.
// Returns 0 on success; otherwise returns the LAPACK info code.
// On singular matrices, returns k>0 where U(k,k)=0 after LU factorization.
__declspec(dllexport)
int invert_matrix(double *A, MKL_INT n) {
    if (!A || n <= 0) return -1;

    MKL_INT info = 0;
    MKL_INT *ipiv = (MKL_INT *)malloc(sizeof(MKL_INT) * n);
    if (!ipiv) return -100;

    // 1) LU factorization: A = P * L * U  (overwrites A)
    info = LAPACKE_dgetrf(LAPACK_ROW_MAJOR, n, n, A, n, ipiv);
    if (info != 0) {  // info < 0: illegal arg; info > 0: singular matrix
        free(ipiv);
        return (int)info;
    }

    // 2) Compute inverse from LU factors (overwrites A with A^{-1})
    info = LAPACKE_dgetri(LAPACK_ROW_MAJOR, n, A, n, ipiv);
    free(ipiv);
    return (int)info; // 0 on success; >0 if U has zero on diagonal
}
