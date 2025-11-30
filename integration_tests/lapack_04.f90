C     Minimal reproducer from LAPACK stfsm.f
C     Tests 0-based assumed-size array with lbound/ubound intrinsics
C
      SUBROUTINE SUB(A, N)
      REAL A(0:*)
      INTEGER N
      INTEGER I, LB
C
C     Use lbound on assumed-size array (should return 0 for dimension 1)
C
      LB = LBOUND(A, 1)
      IF (LB .NE. 0) THEN
         PRINT *, 'FAIL: LBOUND should be 0, got', LB
         ERROR STOP
      END IF
C
C     Simple operation on the array
C
      DO 10 I = 0, N-1
         A(I) = A(I) + 1.0
   10 CONTINUE
      END
C
      PROGRAM LAPACK_04
      REAL X(4)
      INTEGER I
C
      DO 10 I = 1, 4
         X(I) = REAL(I)
   10 CONTINUE
C
      CALL SUB(X, 4)
C
      DO 20 I = 1, 4
         IF (ABS(X(I) - REAL(I+1)) .GT. 1E-6) THEN
            PRINT *, 'FAIL at index', I
            ERROR STOP
         END IF
   20 CONTINUE
C
      PRINT *, 'OK'
      END
