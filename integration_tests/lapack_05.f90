C     Minimal reproducer for descriptor-argument dependency checks
C     Mirrors LAPACK patterns where a slice like A(1, J) relies on LDA.
C     gfortran accepts this; current LFortran emits an ASR dependency error.
      PROGRAM lapack_dependency_test
      INTEGER LDA, N
      PARAMETER (LDA = 8, N = 4)
      REAL A(LDA, N)

      CALL FOO(N, A(1, 2), LDA)
      PRINT *, 'Done'
      END

      SUBROUTINE FOO(M, X, LDA)
      INTEGER M, LDA
      REAL X(LDA, *)
      RETURN
      END
