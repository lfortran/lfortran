C     Minimal reproducer from LAPACK stfsm.f
C     Tests 0-based assumed-size array with lbound/ubound intrinsics
C
      SUBROUTINE SUB(A, N)
      REAL A(0:3,0:*)
      INTEGER N
      INTEGER I, LB, UB
C
C     Use lbound on assumed-size array (should return 0 for dimension 1)
C
      LB = LBOUND(A, 1)
      UB = UBOUND(A, 1)
      IF (LB .NE. 0) THEN
         PRINT *, 'FAIL: LBOUND should be 0, got', LB
         ERROR STOP
      END IF
      IF (UB .LT. 0) THEN
         PRINT *, 'FAIL: UBOUND should be non-negative, got', UB
         ERROR STOP
      END IF
C
C     Simple operation on the array
C
      DO 10 I = 0, N-1
         A(I,0) = A(I,0) + 1.0
   10 CONTINUE
      END
C
      PROGRAM LAPACK_04
      REAL X(0:3,0:0)
      INTEGER I
C
      DO 10 I = 0, 3
         X(I,0) = REAL(I+1)
   10 CONTINUE
C
      CALL SUB(X, 4)
C
      DO 20 I = 0, 3
         IF (ABS(X(I,0) - REAL(I+2)) .GT. 1E-6) THEN
            PRINT *, 'FAIL at index', I
            ERROR STOP
         END IF
   20 CONTINUE
C
      PRINT *, 'OK'
      END
