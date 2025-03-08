      PROGRAM continue_compilation_ff
      INTEGER :: init_x = 1

      INTEGER, POINTER :: y_1
      INTEGER(2), POINTER :: y_2(:)
      INTEGER :: shape(2, 2)
      INTEGER, PARAMETER :: x = 2
      REAL :: circleArea

      COMPLEX :: a
      INTEGER :: val
      CHARACTER(1) :: x_2
      INTEGER :: i
      INTEGER :: a_2(3)
      INTEGER :: size_a
      INTEGER :: a_3(3)
      INTEGER :: size_a_2
      INTEGER :: kindvar = 4
      REAL(8), ALLOCATABLE :: x_3(:)
      REAL :: y_3
      INTEGER, PARAMETER :: Nx = 600, Ny = 450
      INTEGER :: i_1, j, image(Nx, Ny)
      INTEGER :: i_2, j_1
      INTEGER :: i_3
      COMPLEX :: a_4
      COMPLEX :: a_5
      REAL :: y_4
      INTEGER :: I J
      CLOSE(UNIT=200)
      I == 10
      J = 20..5
      PRINT *, "Value of I is" I
      CHARACTER*10 STR = 'Hello  '
      WRITE(*,10 FORMAT(A))
      A = B +  * C
      INTEGER K L M = 5
      READ *, N 5
      REAL X Y Z 10.5
      COMPLEX C = (1.0,2.0
      WRITE(*,10) FORMAT(A, I5)
      I === 10
      J = .20E
      REAL*8 A = 1.23.45
      INTEGER*4 VAR/5/
      COMMON /BLOCK/ A, B C
      INTEGER*4 VAR/5/
      EQUIVALENCE (X Y)
      DO 20 I = 1 10

      val = THIS_IMAGE()
      CALL CO_SUM(val, RESULT_IMAGE=1)
      IF (THIS_IMAGE() == 1) THEN
        WRITE(*,*) "The sum is ", val
      END IF

      CALL EVENT_QUERY(1, 1, 1)
      x_2 = 'u'
      i = 10
      IF (i > ICHAR(x_2)) THEN
      END IF

      size_a = SIZE(a_2, DIM=1)
      size_a_2 = SIZE(a_3, KIND=kindvar, DIM=1)

      PRINT *, ["a", "b", "ball", "cat"]
      PRINT *, [1, 2.]
      PRINT *, [1, [1., 2.]]
      PRINT *, DFLOAT(y_3)

      PRINT *, SUM([1, 2, 3], 1.1)
      PRINT *, DINT(1.0_8, 8)
      PRINT *, DLGAMA(2.7)
      PRINT *, DNINT(1.0_8, 8)
      PRINT *, DPROD(4.23_8, 4.3_8)

      DO i_2 = 1, 10
        DO j_1 = 1, 2
          i_2 = j_1 + 1
        END DO
        j_1 = i_2 + 1
        PRINT *, i_2, j_1
      END DO

      DO i_2 = 1, 5, 0
        WRITE(*,*) i_3
      END DO

      PRINT *, DREAL(a_4)
      a_5 = CMPLX(1)
      PRINT *, FLOAT(y_4)

      CONTAINS

      LOGICAL FUNCTION f(x)
      INTEGER, INTENT(IN), OPTIONAL :: x
      f = PRESENT(x)
      END FUNCTION f

      END PROGRAM continue_compilation_ff
