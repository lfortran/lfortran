C     EXACT SORM22 from LAPACK - reproduces the compilation error
      SUBROUTINE SORM22( SIDE, TRANS, M, N, N1, N2, Q, LDQ, C, LDC,
     $                   WORK, LWORK, INFO )
      IMPLICIT NONE
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            M, N, N1, N2, LDQ, LDC, LWORK, INFO
*     ..
*     .. Array Arguments ..
      REAL               Q( LDQ, * ), C( LDC, * ), WORK( * )
*     ..
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, LDWORK, LEN, LWKOPT, NB, NQ, NW
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               SROUNDUP_LWORK
      EXTERNAL           LSAME, SROUNDUP_LWORK
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEMM, SLACPY, STRMM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )

      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      NW = NQ
      IF( N1.EQ.0 .OR. N2.EQ.0 ) NW = 1
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ) .AND.
     $         .NOT.LSAME( TRANS, 'T' ) )
     $          THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( N1.LT.0 .OR. N1+N2.NE.NQ ) THEN
         INFO = -5
      ELSE IF( N2.LT.0 ) THEN
         INFO = -6
      ELSE IF( LDQ.LT.MAX( 1, NQ ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.NW .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF

      IF( INFO.EQ.0 ) THEN
         LWKOPT = M*N
         WORK( 1 ) = SROUNDUP_LWORK( LWKOPT )
      END IF

      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORM22', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF

      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF

      IF( N1.EQ.0 ) THEN
         CALL STRMM( SIDE, 'Upper', TRANS, 'Non-Unit', M, N, ONE,
     $               Q, LDQ, C, LDC )
         WORK( 1 ) = ONE
         RETURN
      ELSE IF( N2.EQ.0 ) THEN
         CALL STRMM( SIDE, 'Lower', TRANS, 'Non-Unit', M, N, ONE,
     $               Q, LDQ, C, LDC )
         WORK( 1 ) = ONE
         RETURN
      END IF

      NB = MAX( 1, MIN( LWORK, LWKOPT ) / NQ )

      IF( LEFT ) THEN
         IF( NOTRAN ) THEN
            DO I = 1, N, NB
               LEN = MIN( NB, N-I+1 )
               LDWORK = M

C              Multiply bottom part of C by Q12.
               CALL SLACPY( 'All', N1, LEN, C( N2+1, I ), LDC, WORK,
     $                      LDWORK )
C              THIS IS LINE 287 FROM ORIGINAL SORM22.F - THE ERROR LINE!
               CALL STRMM( 'Left', 'Lower', 'No Transpose',
     $                     'Non-Unit',
     $                     N1, LEN, ONE, Q( 1, N2+1 ), LDQ, WORK,
     $                     LDWORK )

C              Multiply top part of C by Q11.
               CALL SGEMM( 'No Transpose', 'No Transpose', N1, LEN,
     $                     N2,
     $                     ONE, Q, LDQ, C( 1, I ), LDC, ONE, WORK,
     $                     LDWORK )

C              Multiply top part of C by Q21.
               CALL SLACPY( 'All', N2, LEN, C( 1, I ), LDC,
     $                      WORK( N1+1 ), LDWORK )
               CALL STRMM( 'Left', 'Upper', 'No Transpose',
     $                     'Non-Unit',
     $                     N2, LEN, ONE, Q( N1+1, 1 ), LDQ,
     $                     WORK( N1+1 ), LDWORK )

C              Multiply bottom part of C by Q22.
               CALL SGEMM( 'No Transpose', 'No Transpose', N2, LEN,
     $                     N1,
     $                     ONE, Q( N1+1, N2+1 ), LDQ, C( N2+1, I ), LDC,
     $                     ONE, WORK( N1+1 ), LDWORK )

C              Copy everything back.
               CALL SLACPY( 'All', M, LEN, WORK, LDWORK, C( 1, I ),
     $                      LDC )
            END DO
         END IF
      END IF
      RETURN
      END

C     Test program
      PROGRAM TEST_SORM22
      IMPLICIT NONE
      INTEGER M, N, N1, N2, LDQ, LDC, LWORK, INFO
      PARAMETER (M = 10, N = 10, N1 = 5, N2 = 5)
      PARAMETER (LDQ = 10, LDC = 10, LWORK = 100)
      REAL Q(LDQ, M), C(LDC, N), WORK(LWORK)

      CALL SORM22('L', 'N', M, N, N1, N2, Q, LDQ, C, LDC,
     $            WORK, LWORK, INFO)

      PRINT *, 'INFO =', INFO
      END PROGRAM

C     Dummy routines
      LOGICAL FUNCTION LSAME(CA, CB)
      CHARACTER CA, CB
      LSAME = CA .EQ. CB
      END

      REAL FUNCTION SROUNDUP_LWORK(LWORK)
      INTEGER LWORK
      SROUNDUP_LWORK = REAL(LWORK)
      END

      SUBROUTINE XERBLA(SRNAME, INFO)
      CHARACTER*(*) SRNAME
      INTEGER INFO
      PRINT *, 'Error in ', SRNAME, ', argument', -INFO
      STOP
      END

      SUBROUTINE SLACPY(UPLO, M, N, A, LDA, B, LDB)
      CHARACTER UPLO
      INTEGER M, N, LDA, LDB
      REAL A(LDA, *), B(LDB, *)
      RETURN
      END

      SUBROUTINE STRMM(SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, LDA,
     +                  B, LDB)
      CHARACTER SIDE, UPLO, TRANS, DIAG
      INTEGER M, N, LDA, LDB
      REAL ALPHA, A(LDA, *), B(LDB, *)
      RETURN
      END

      SUBROUTINE SGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                  B, LDB, BETA, C, LDC)
      CHARACTER TRANSA, TRANSB
      INTEGER M, N, K, LDA, LDB, LDC
      REAL ALPHA, BETA, A(LDA, *), B(LDB, *), C(LDC, *)
      RETURN
      END