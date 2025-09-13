      SUBROUTINE STFSM( M, N, A, B, LDB )
      IMPLICIT NONE
      INTEGER            LDB, M, N
      REAL               A( 0: * ), B( 0: LDB-1, 0: * )
      LOGICAL            LOWER, MISODD, NORMALTRANSR, NOTRANS
      INTEGER            M1, M2
      EXTERNAL           STRSM

      NORMALTRANSR = .TRUE.
      LOWER = .TRUE.
      NOTRANS = .TRUE.

      IF( MOD( M, 2 ).EQ.0 ) THEN
         MISODD = .FALSE.
      ELSE
         MISODD = .TRUE.
         M2 = M / 2
         M1 = M - M2
      END IF

      IF( MISODD ) THEN
         IF( NORMALTRANSR ) THEN
            IF( LOWER ) THEN
               IF( NOTRANS ) THEN
                  IF( M.EQ.1 ) THEN
                     CALL STRSM( 'L', 'L', 'N', 'N', M1, N,
     $                           1.0, A, M, B, LDB )
                  ELSE
                     CALL STRSM( 'L', 'L', 'N', 'N', M1, N,
     $                           1.0, A( 0 ), M, B, LDB )
                     CALL STRSM( 'L', 'U', 'T', 'N', M2, N, 1.0,
     $                           A( M ), M, B( M1, 0 ), LDB )
                  END IF
               END IF
            END IF
         END IF
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
      PRINT *, 'Test completed'
      END PROGRAM TEST