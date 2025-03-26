c     If you need a function, put it into the module below and remove the same
c     number of lines below the module to keep the rest of the lines in this file
c     intact.     
      module continue_compilation_1_mod
      CONTAINS

      LOGICAL FUNCTION f(x)
      INTEGER, INTENT(IN), OPTIONAL :: x
      f = PRESENT(x)
      END FUNCTION f
      end module


















c     Only put declarations and statements here, no subroutines (those go above).
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




























c     Use the space above to insert new declarations, and remove the line, so
c     that the lines below do not shift, to keep the diff minimal.
c     Only put statements below. If you need to call a function, put it into a
c     module above.      

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
      I === 10
      J = .20E
      REAL*8 A = 1.23.45
      INTEGER*4 VAR/5/
      INTEGER*4 VAR/5/
      EQUIVALENCE (X Y)

      x_2 = 'u'
      i = 10
      IF (i > ICHAR(x_2)) THEN
      END IF

      size_a = SIZE(a_2, DIM=1)

      a_5 = CMPLX(1)
      IF I .EQ. 10 THEN PRINT *, "Ten"
      CALL FUNC( 5, 6,

      CONTAINS

      LOGICAL FUNCTION f(x)
      INTEGER, INTENT(IN), OPTIONAL :: x
      f = PRESENT(x)
      END FUNCTION f

      END PROGRAM continue_compilation_ff
