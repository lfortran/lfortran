      SUBROUTINE STFSM( M, N, A, B, LDB )
      IMPLICIT NONE
      INTEGER            LDB, M, N
      REAL               A( 0: * ), B( 0: LDB-1, 0: * )
      EXTERNAL           STRSM

      IF( M.EQ.1 ) THEN
         CALL STRSM( M, N, A, M, B, LDB )
      ELSE
         CALL STRSM( M, N, A( 0 ), M, B, LDB )
      END IF

      RETURN
      END

      SUBROUTINE STRSM( M1, N, ARR, M, B, LDB )
      IMPLICIT NONE
      INTEGER            M1, N, M, LDB
      REAL               ARR( 0: * ), B( 0: LDB-1, 0: * )

      IF( M1 .GT. 0 ) THEN
         ARR(0) = 3.14
      END IF
      RETURN
      END

      PROGRAM TEST
      REAL A(100), B(10, 10)
      INTEGER LDB
      LDB = 10
      A = 1.0
      B = 2.0
      CALL STFSM(3, 3, A, B, LDB)
      IF (ABS(A(1) - 3.14) .LT. 0.001) THEN
         PRINT *, 'PASS'
      ELSE
         PRINT *, 'FAIL: A(1) =', A(1)
      END IF
      END PROGRAM TEST