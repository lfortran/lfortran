subroutine dlaswp(n, a, lda, k1, k2, ipiv, incx)
      INTEGER            INCX, K1, K2, LDA, N
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
      INTEGER            I, I1, I2, INC, IP, IX, IX0, J, K, N32
      DOUBLE PRECISION   TEMP
      IF( incx > 0 ) THEN
         ix0 = k1
         i1 = k1
         i2 = k2
         inc = 1
      ELSE IF( incx < 0 ) THEN
         ix0 = k1 + ( k1-k2 )*incx
         i1 = k2
         i2 = k1
         inc = -1
      ELSE
         RETURN
      END IF
      n32 = ( n / 32 )*32
      IF( n32 /= 0 ) THEN
         DO 30 j = 1, n32, 32
            ix = ix0
            DO 20 i = i1, i2, inc
               ip = ipiv( ix )
               IF( ip /= i ) THEN
                  DO 10 k = j, j + 31
                     temp = a( i, k )
                     a( i, k ) = a( ip, k )
                     a( ip, k ) = temp
   10             CONTINUE
               END IF
               ix = ix + incx
   20       CONTINUE
   30    CONTINUE
      END IF
      IF( n32 /= n ) THEN
         n32 = n32 + 1
         ix = ix0
         DO 50 i = i1, i2, inc
            ip = ipiv( ix )
            IF( ip /= i ) THEN
               DO 40 k = n32, n
                  temp = a( i, k )
                  a( i, k ) = a( ip, k )
                  a( ip, k ) = temp
   40          CONTINUE
            END IF
            ix = ix + incx
   50    CONTINUE
      END IF
   RETURN
end subroutine

PROGRAM test_dlaswp
   INTEGER :: n, lda, k1, k2, incx, i
   INTEGER, ALLOCATABLE :: ipiv(:)
   DOUBLE PRECISION, ALLOCATABLE :: a(:,:)

   ! Define the input values
   n = 4
   lda = 4
   k1 = 1
   k2 = 4
   incx = 1

   ! Allocate the arrays
   ALLOCATE(ipiv(n))
   ALLOCATE(a(lda, n))

    ! Initialize the matrix 'a' in column-major order
    a = RESHAPE([ 1.0D0, 2.0D0, 3.0D0, 4.0D0, &
                 5.0D0, 6.0D0, 7.0D0, 8.0D0, &
                 9.0D0, 10.0D0, 11.0D0, 12.0D0, &
                 13.0D0, 14.0D0, 15.0D0, 16.0D0 ], [ lda, n ])

    ! Initialize the pivot array
    ipiv = [4, 1, 2, 3]

   ! Print the original matrix
   PRINT *, "Original matrix A:"
   DO i = 1, lda
       PRINT *, a(i, :)
   END DO

   ! Call the subroutine
   CALL dlaswp(n, a, lda, k1, k2, ipiv, incx)

   ! Print the modified matrix
   PRINT *, "Modified matrix A:"
   DO i = 1, lda
       PRINT *, a(i, :)
   END DO
END PROGRAM test_dlaswp
