program c_sizeof_04
  use iso_c_binding, only: c_int, c_double, c_sizeof
  implicit none

  ! bind(C) type where declaration order causes padding
  ! C equivalent: struct t1 { int a; double d; int b; };
  ! sizeof = 4 + 4(pad) + 8 + 4 + 4(pad) = 24
  type, bind(C) :: t1
    integer(c_int) :: a
    real(c_double) :: d
    integer(c_int) :: b
  end type

  ! bind(C) type with no padding needed (fields already naturally packed)
  ! C equivalent: struct t2 { double d; int a; int b; };
  ! sizeof = 8 + 4 + 4 = 16
  type, bind(C) :: t2
    real(c_double) :: d
    integer(c_int) :: a
    integer(c_int) :: b
  end type

  type(t1) :: x1
  type(t2) :: x2

  print *, c_sizeof(x1)
  print *, c_sizeof(x2)

  if (c_sizeof(x1) /= 24) error stop "FAIL: c_sizeof(t1) should be 24"
  if (c_sizeof(x2) /= 16) error stop "FAIL: c_sizeof(t2) should be 16"
  print *, "PASS"
end program
