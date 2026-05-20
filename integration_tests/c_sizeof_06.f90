program c_sizeof_06
  use iso_c_binding, only: c_int, c_double, c_sizeof
  implicit none

  type, bind(C) :: t1
    integer(c_int) :: arr(2)
  end type

  type, bind(C) :: t2
    integer(c_int) :: a
    integer(c_int) :: arr(3)
    integer(c_int) :: b
  end type

  type, bind(C) :: t3
    real(c_double) :: d
    integer(c_int) :: arr(4)
  end type

  type(t1) :: x1
  type(t2) :: x2
  type(t3) :: x3
  integer(c_int) :: yi
  real(c_double) :: yd

  ! t1: 2 * c_int = 8
  print *, "c_sizeof(t1) =", c_sizeof(x1)
  if (c_sizeof(x1) /= 2 * c_sizeof(yi)) error stop 1

  ! t2: 5 * c_int = 20
  print *, "c_sizeof(t2) =", c_sizeof(x2)
  if (c_sizeof(x2) /= 5 * c_sizeof(yi)) error stop 2

  ! t3: c_double + 4*c_int = 8 + 16 = 24
  print *, "c_sizeof(t3) =", c_sizeof(x3)
  if (c_sizeof(x3) /= c_sizeof(yd) + 4 * c_sizeof(yi)) error stop 3

  print *, "PASS"
end program
