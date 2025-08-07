      SUBROUTINE SINCOS(A, S, C)
      DOUBLE PRECISION A, S, C

      S = DSIN(A)
      C = DCOS(A)

      RETURN
      END

      SUBROUTINE SINCOS_ARRAY(N, A, S, C)
      INTEGER N
      DOUBLE PRECISION A(N), S(N), C(N)
      INTEGER I

      DO 10 I = 1, N
         S(I) = DSIN(A(I))
         C(I) = DCOS(A(I))
 10   CONTINUE

      RETURN
      END

      SUBROUTINE SINCOS_A2(N, A, CS)
      INTEGER N
      DOUBLE PRECISION A(N), CS(N * 2)
      INTEGER I

      DO 20 I = 1, N
         CS(2 * I)     = DSIN(A(I))
         CS(2 * I - 1) = DCOS(A(I))
 20   CONTINUE

      RETURN
      END
