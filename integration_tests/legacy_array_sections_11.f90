      PROGRAM LEGACY_ARRAY_SECTIONS_11
      CHARACTER          TRANS
      INTEGER            IRESID, LDA, LDB, LDX, LWORK, M, N, NRHS
      REAL               A( 1, 1 ), B( 1, 1 ), C( 1, 1 ), WORK( 1 ),
     $                   X( 1, 1 )
      REAL               SQRT17, R

      TRANS = 'N'
      IRESID = 1
      M = 0
      N = 1
      NRHS = 1
      LDA = 1
      LDB = 1
      LDX = 1
      LWORK = 1

      R = SQRT17( TRANS, IRESID, M, N, NRHS, A, LDA, X, LDX, B, LDB,
     $     C, WORK, LWORK )

      IF( R.NE.0.0E0 ) STOP 1

      END

      REAL             FUNCTION SQRT17( TRANS, IRESID, M, N, NRHS, A,
     $                 LDA, X, LDX, B, LDB, C, WORK, LWORK )
      CHARACTER          TRANS
      INTEGER            IRESID, LDA, LDB, LDX, LWORK, M, N, NRHS
      REAL               A( LDA, * ), B( LDB, * ), C( LDB, * ),
     $                   WORK( LWORK ), X( LDX, * )
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
      INTEGER            INFO, ISCL, NCOLS, NROWS
      REAL               ERR, NORMA, NORMB, NORMRS, SMLNUM
      REAL               RWORK( 1 )
      LOGICAL            LSAME
      REAL               SLAMCH, SLANGE
      EXTERNAL           LSAME, SLAMCH, SLANGE
      EXTERNAL           SGEMM, SLACPY, SLASCL, XERBLA
      INTRINSIC          MAX, REAL

      SQRT17 = ZERO

      IF( LSAME( TRANS, 'N' ) ) THEN
         NROWS = M
         NCOLS = N
      ELSE IF( LSAME( TRANS, 'T' ) ) THEN
         NROWS = N
         NCOLS = M
      ELSE
         CALL XERBLA( 'SQRT17', 1 )
         RETURN
      END IF

      IF( LWORK.LT.NCOLS*NRHS ) THEN
         CALL XERBLA( 'SQRT17', 13 )
         RETURN
      END IF

      IF( M.LE.0 .OR. N.LE.0 .OR. NRHS.LE.0 ) THEN
         RETURN
      END IF

      NORMA = SLANGE( 'One-norm', M, N, A, LDA, RWORK )
      SMLNUM = SLAMCH( 'Safe minimum' ) / SLAMCH( 'Precision' )
      ISCL = 0

      IF( NORMA.GT.ZERO .AND. NORMA.LT.SMLNUM ) THEN
         ISCL = 1
         CALL SLASCL( 'General', 0, 0, NORMA, SMLNUM, M, N, A, LDA,
     $                INFO )
      END IF

      CALL SLACPY( 'All', NROWS, NRHS, B, LDB, C, LDB )

      CALL SGEMM( 'Transpose', TRANS, NRHS, NCOLS, NROWS, ONE, C, LDB,
     $            A, LDA, ZERO, WORK, NRHS )

      ERR = SLANGE( 'One-norm', NRHS, NCOLS, WORK, NRHS, RWORK )

      NORMB = SLANGE( 'One-norm', NROWS, NRHS, B, LDB, RWORK )

      IF( NORMB.NE.ZERO ) THEN
         NORMRS = ERR / NORMB
      ELSE
         NORMRS = ERR
      END IF

      IF( ISCL.EQ.1 ) THEN
         CALL SLASCL( 'General', 0, 0, SMLNUM, NORMA, M, N, A, LDA,
     $                INFO )
      END IF

      IF( NORMRS.GT.SQRT17 ) THEN
         SQRT17 = NORMRS
      END IF

      RETURN

      END
