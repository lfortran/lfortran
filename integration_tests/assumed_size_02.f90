! Test case for assumed-size array type mismatch bug
! Demonstrates issue with 2D array element B(i,j) passed as 1D array starting point
! Derived from LAPACK's slals0.f subroutine where this pattern causes compilation failure
!
! License Information (from original LAPACK):
! -- LAPACK is a software package provided by Univ. of Tennessee,    --
! -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--

      SUBROUTINE SLALS0( ICOMPQ, NL, NR, SQRE, NRHS, B, LDB, BX, &
                         LDBX, &
                         PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM, &
                         POLES, DIFL, DIFR, Z, K, C, S, WORK, INFO )
      IMPLICIT NONE
      INTEGER            GIVPTR, ICOMPQ, INFO, K, LDB, LDBX, LDGCOL, &
                         LDGNUM, NL, NR, NRHS, SQRE
      REAL               C, S
      INTEGER            GIVCOL( LDGCOL, * ), PERM( * )
      REAL               B( LDB, * ), BX( LDBX, * ), DIFL( * ), &
                         DIFR( LDGNUM, * ), GIVNUM( LDGNUM, * ), &
                         POLES( LDGNUM, * ), WORK( * ), Z( * )

      REAL               ONE, ZERO, NEGONE
      PARAMETER          ( ONE = 1.0E0, ZERO = 0.0E0, NEGONE = -1.0E0 )
      
      INTEGER            I, J, M, N, NLP1
      REAL               DIFLJ, DIFRJ, DJ, DSIGJ, DSIGJP, TEMP
      LOGICAL            LSAME
      EXTERNAL           LSAME
      EXTERNAL           SCOPY, SGEMV, SLACPY, SLASCL, SROT, SSCAL, XERBLA
      INTRINSIC          REAL
      
      INFO = 0
      N = NL + NR + 1
      
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( NL.LT.1 ) THEN
         INFO = -2
      ELSE IF( NR.LT.1 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.1 ) THEN
         INFO = -5
      ELSE IF( LDB.LT.N ) THEN
         INFO = -7
      ELSE IF( LDBX.LT.N ) THEN
         INFO = -9
      ELSE IF( GIVPTR.LT.0 ) THEN
         INFO = -11
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -13
      ELSE IF( LDGNUM.LT.N ) THEN
         INFO = -15
      ELSE IF( K.LT.1 ) THEN
         INFO = -20
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SLALS0', -INFO )
         RETURN
      END IF
      
      M = N + SQRE
      NLP1 = NL + 1
      
      IF( ICOMPQ.EQ.0 ) THEN
         ! Apply back orthogonal transformations from the left.
         ! Step (1L): apply back the Givens rotations performed.
         DO I = 1, GIVPTR
            ! This is the problematic call that causes type mismatch
            ! B(GIVCOL(I,2), 1) and B(GIVCOL(I,1), 1) are treated as 
            ! starting points for 1D arrays in Fortran convention
            CALL SROT( NRHS, B( GIVCOL( I, 2 ), 1 ), LDB, &
                       B( GIVCOL( I, 1 ), 1 ), LDB, GIVNUM( I, 2 ), &
                       GIVNUM( I, 1 ) )
         END DO
         
         ! Step (2L): permute rows of B.
         CALL SCOPY( NRHS, B( NLP1, 1 ), LDB, BX( 1, 1 ), LDBX )
         DO I = 2, N
            CALL SCOPY( NRHS, B( PERM( I ), 1 ), LDB, BX( I, 1 ), &
                        LDBX )
         END DO
      END IF
      
      RETURN
      END SUBROUTINE SLALS0

      ! Stub implementations for external routines
      SUBROUTINE SROT(N, SX, INCX, SY, INCY, C, S)
      INTEGER INCX, INCY, N
      REAL C, S
      REAL SX(*), SY(*)
      ! Simplified BLAS SROT implementation
      INTEGER I, IX, IY
      REAL STEMP
      
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
         DO I = 1,N
            STEMP = C*SX(I) + S*SY(I)
            SY(I) = C*SY(I) - S*SX(I)
            SX(I) = STEMP
         END DO
      ELSE
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            STEMP = C*SX(IX) + S*SY(IY)
            SY(IY) = C*SY(IY) - S*SX(IX)
            SX(IX) = STEMP
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
      END SUBROUTINE SROT
      
      SUBROUTINE SCOPY(N, SX, INCX, SY, INCY)
      INTEGER INCX, INCY, N
      REAL SX(*), SY(*)
      ! Simplified BLAS SCOPY implementation
      INTEGER I, IX, IY
      
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
         DO I = 1,N
            SY(I) = SX(I)
         END DO
      ELSE
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            SY(IY) = SX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
      END SUBROUTINE SCOPY
      
      SUBROUTINE XERBLA(SRNAME, INFO)
      CHARACTER*(*) SRNAME
      INTEGER INFO
      PRINT *, 'Error in ', SRNAME, ' INFO=', INFO
      STOP
      END SUBROUTINE XERBLA

      ! Test driver program
      PROGRAM test_slals0
      IMPLICIT NONE
      INTEGER :: N, NRHS, LDB, LDBX, LDGCOL, LDGNUM
      PARAMETER (N=4, NRHS=2, LDB=10, LDBX=10, LDGCOL=10, LDGNUM=10)
      
      REAL :: B(LDB, NRHS), BX(LDBX, NRHS)
      INTEGER :: GIVCOL(LDGCOL, 2), PERM(N)
      REAL :: GIVNUM(LDGNUM, 2), POLES(LDGNUM, 2)
      REAL :: DIFL(N), DIFR(LDGNUM, 2), Z(N), WORK(N)
      REAL :: C, S
      INTEGER :: ICOMPQ, NL, NR, SQRE, GIVPTR, K, INFO
      INTEGER :: I, J
      
      ! Initialize test data
      NL = 2
      NR = 1
      SQRE = 0
      ICOMPQ = 0
      GIVPTR = 1
      K = 2
      C = 0.8
      S = 0.6
      
      ! Initialize arrays with test values
      DO J = 1, NRHS
         DO I = 1, N
            B(I, J) = REAL(I + J)
         END DO
      END DO
      
      GIVCOL(1, 1) = 1
      GIVCOL(1, 2) = 2
      
      GIVNUM(1, 1) = 0.8
      GIVNUM(1, 2) = 0.6
      
      DO I = 1, N
         PERM(I) = I
      END DO
      
      PRINT *, 'Testing SLALS0 with assumed-size arrays...'
      CALL SLALS0(ICOMPQ, NL, NR, SQRE, NRHS, B, LDB, BX, LDBX, &
                  PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM, &
                  POLES, DIFL, DIFR, Z, K, C, S, WORK, INFO)
      
      IF (INFO .EQ. 0) THEN
         PRINT *, 'SLALS0 completed successfully'
      ELSE
         PRINT *, 'SLALS0 failed with INFO=', INFO
         ERROR STOP
      END IF
      
      END PROGRAM test_slals0