C MRE: implicit-interface array physical type mismatch from LAPACK SLALS0
      PROGRAM LAPACK_04
      CALL TEST
      PRINT *, 'PASS'
      END

      SUBROUTINE TEST
      INTEGER PERM(2)
      REAL B(3,1), BX(3,1), WORK(2)
      PERM(1) = 1
      PERM(2) = 2
      CALL SLALS0_MRE(1, B, 3, BX, 3, PERM, 2, WORK, 1, 0)
      END

      SUBROUTINE SLALS0_MRE(ICOMPQ, B, LDB, BX, LDBX, PERM, K, WORK,
     &     NRHS, INFO)
      IMPLICIT NONE
      INTEGER ICOMPQ, LDB, LDBX, K, NRHS, INFO
      INTEGER PERM(*)
      REAL B(LDB,*), BX(LDBX,*), WORK(*)
      REAL TEMP
      IF (ICOMPQ .EQ. 0) THEN
         WORK(1) = -1.0
         TEMP = SNRM2(K, WORK, 1)
         CALL SGEMV('T', K, NRHS, 1.0, BX, LDBX, WORK, 1, 0.0,
     &        B(1,1), LDB)
         CALL SLASCL('G', 0, 0, TEMP, 1.0, 1, NRHS, B(1,1), LDB, INFO)
      ELSE
         CALL SGEMV('T', K, NRHS, 1.0, B, LDB, WORK, 1, 0.0,
     &        BX(1,1), LDBX)
         CALL SCOPY(NRHS, BX(1,1), LDBX, B(PERM(1),1), LDB)
      END IF
      END

      REAL FUNCTION SNRM2(N, X, INCX)
      INTEGER N, INCX
      REAL X(*)
      SNRM2 = 0.0
      END

      SUBROUTINE SGEMV(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, LDY)
      CHARACTER*(*) TRANS
      INTEGER M, N, LDA, INCX, LDY
      REAL ALPHA, BETA
      REAL A(LDA,*), X(*), Y(LDY,*)
      END

      SUBROUTINE SLASCL(TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO)
      CHARACTER*(*) TYPE
      INTEGER KL, KU, M, N, LDA, INFO
      REAL CFROM, CTO, A(LDA,*)
      END

      SUBROUTINE SCOPY(N, X, INCX, Y, INCY)
      INTEGER N, INCX, INCY
      REAL X(*), Y(*)
      END
