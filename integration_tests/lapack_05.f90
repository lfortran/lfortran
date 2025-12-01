      SUBROUTINE STFSM( TRANSR, SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A,
     $                  B, LDB )
*
*     Minimal reproducer derived from LAPACK STFSM:
*       - Fortran 77 fixed-form
*       - A stored in RFP format with 0-based indexing
*       - B has shape B(0:LDB-1,0:*)
*       - Focus on SIDE = 'L', UPLO = 'L', TRANSR = 'N', TRANS = 'N'
*
      CHARACTER          TRANSR, DIAG, SIDE, TRANS, UPLO
      INTEGER            LDB, M, N
      REAL               ALPHA
      REAL               A( 0: * ), B( 0: LDB-1, 0: * )
*
*     Parameters
*
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*
*     Local scalars
*
      LOGICAL            LOWER, LSIDE, MISODD, NORMALTRANSR, NOTRANS
      INTEGER            M1, M2, K, I, J, INFO
*
*     External BLAS/LAPACK helpers
*
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           SGEMM, STRSM, XERBLA
      INTRINSIC          MOD, MAX
*
*     Test input parameters (subset of original STFSM)
*
      INFO = 0
      NORMALTRANSR = LSAME( TRANSR, 'N' )
      LSIDE        = LSAME( SIDE,   'L' )
      LOWER        = LSAME( UPLO,   'L' )
      NOTRANS      = LSAME( TRANS,  'N' )

      IF( .NOT.LSIDE ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'STFSM ', -INFO )
         RETURN
      END IF
*
*     Quick return for empty problems
*
      IF( ( M.EQ.0 ) .OR. ( N.EQ.0 ) ) RETURN
*
*     Quick return when ALPHA = 0: set B to zero, as in LAPACK.
*
      IF( ALPHA.EQ.ZERO ) THEN
         DO 20 J = 0, N-1
            DO 10 I = 0, M-1
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     From here on we mirror the LAPACK STFSM branch
*     SIDE = 'L', NORMALTRANSR, UPLO = 'L', TRANS = 'N'.
*
      IF( LSIDE ) THEN
*
*        A is M-by-M. Determine the splitting of M.
*
         IF( MOD( M, 2 ).EQ.0 ) THEN
            MISODD = .FALSE.
            K = M / 2
         ELSE
            MISODD = .TRUE.
            IF( LOWER ) THEN
               M2 = M / 2
               M1 = M - M2
            ELSE
               M1 = M / 2
               M2 = M - M1
            END IF
         END IF
*
         IF( MISODD ) THEN
*
*           SIDE = 'L' and M is odd
*
            IF( NORMALTRANSR ) THEN
*
*              TRANSR = 'N'
*
               IF( LOWER ) THEN
*
*                 UPLO = 'L'
*
                  IF( NOTRANS ) THEN
*
*                    TRANS = 'N'
*
                     IF( M.EQ.1 ) THEN
                        CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA,
     $                              A, M, B, LDB )
                     ELSE
                        CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA,
     $                              A( 0 ), M, B, LDB )
                        CALL SGEMM( 'N', 'N', M2, N, M1, -ONE, A( M1 ),
     $                              M, B, LDB, ALPHA, B( M1, 0 ), LDB )
                        CALL STRSM( 'L', 'U', 'T', DIAG, M2, N, ONE,
     $                              A( M ), M, B( M1, 0 ), LDB )
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

      RETURN
      END

      SUBROUTINE STRSM( SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, LDA,
     $                  B, LDB )
      CHARACTER          SIDE, UPLO, TRANS, DIAG
      INTEGER            M, N, LDA, LDB
      REAL               ALPHA
      REAL               A( LDA, * ), B( LDB, * )
*
*     Naive reference-style STRSM: only the cases used by STFSM are
*     exercised in this test (SIDE = 'L', UPLO = 'L' or 'U').
*
      INTEGER            I, J, K
      LOGICAL            LUNIT

      LUNIT = ( DIAG.EQ.'U' )

      IF( SIDE.EQ.'L' ) THEN
         IF( UPLO.EQ.'L' ) THEN
*
*           Solve L * X = ALPHA * B  where L is lower triangular.
*
            DO 40 J = 1, N
               DO 30 I = 1, M
                  B( I, J ) = ALPHA * B( I, J )
                  DO 20 K = 1, I-1
                     B( I, J ) = B( I, J ) - A( I, K ) * B( K, J )
   20             CONTINUE
                  IF( .NOT.LUNIT ) THEN
                     B( I, J ) = B( I, J ) / A( I, I )
                  END IF
   30          CONTINUE
   40       CONTINUE
         ELSE
*
*           Solve U * X = ALPHA * B  where U is upper triangular.
*
            DO 80 J = 1, N
               DO 70 I = M, 1, -1
                  B( I, J ) = ALPHA * B( I, J )
                  DO 60 K = I+1, M
                     B( I, J ) = B( I, J ) - A( I, K ) * B( K, J )
   60             CONTINUE
                  IF( .NOT.LUNIT ) THEN
                     B( I, J ) = B( I, J ) / A( I, I )
                  END IF
   70          CONTINUE
   80       CONTINUE
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SGEMM( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                  BETA, C, LDC )
      CHARACTER          TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      REAL               ALPHA, BETA
      REAL               A( * ), B( * ), C( * )
*
*     Minimal stub: arguments and calling pattern match LAPACK SGEMM,
*     but no computation is performed. This is sufficient to exercise
*     STFSM's array sections and sequence association.
*
      RETURN
      END

      LOGICAL FUNCTION LSAME( CA, CB )
      CHARACTER          CA, CB
      INTEGER            ICA, ICB
*
*     Case-insensitive character compare, sufficient for STFSM tests.
*
      ICA = ICHAR( CA )
      ICB = ICHAR( CB )
*
*     Convert to upper case if needed
*
      IF( ( ICA.GE.97 ) .AND. ( ICA.LE.122 ) ) THEN
         ICA = ICA - 32
      END IF
      IF( ( ICB.GE.97 ) .AND. ( ICB.LE.122 ) ) THEN
         ICB = ICB - 32
      END IF

      LSAME = ( ICA.EQ.ICB )

      RETURN
      END

      SUBROUTINE XERBLA( SRNAME, INFO )
      CHARACTER*(*)      SRNAME
      INTEGER            INFO
*
*     Simple error handler: report and stop.
*
      PRINT *, 'XERBLA called from', SRNAME, 'INFO =', INFO
      STOP
      END

      PROGRAM LAPACK05_MAIN
      CHARACTER          TRANSR, SIDE, UPLO, TRANS, DIAG
      INTEGER            M, N, LDB
      REAL               ALPHA
      INTEGER            LDA
      PARAMETER         ( LDA = 1 )
      REAL               A( 0: LDA-1 ), B( 0: 0, 0: LDA-1 )
*
*     Drive STFSM on a tiny problem that still exercises the
*     LSIDE = .TRUE., LOWER = .TRUE., NORMALTRANSR = .TRUE., NOTRANS path.
*
      TRANSR = 'N'
      SIDE   = 'L'
      UPLO   = 'L'
      TRANS  = 'N'
      DIAG   = 'N'
      M      = 1
      N      = 1
      LDB    = 1
      ALPHA  = 1.0E+0
      A(0)   = 1.0E+0
      B(0,0) = 2.0E+0

      CALL STFSM( TRANSR, SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, B,
     $            LDB )

      END
