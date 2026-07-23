! Passing an array element to a VALUE dummy of a BIND(C) procedure.
! Regression test: LFortran used to compute the element address and emit an
! invalid `bitcast i32* to i32` instead of loading the scalar value, so the
! generated module failed LLVM verification.
module bindc_54_mod
  use iso_c_binding, only: c_int, c_double
  implicit none
  integer(c_int) :: last_int = 0
  real(c_double) :: last_dbl = 0.0d0
contains
  subroutine take_int(x) bind(c)
    integer(c_int), value :: x
    last_int = x
  end subroutine take_int

  subroutine take_int_in(x) bind(c)
    integer(c_int), value, intent(in) :: x
    last_int = x
  end subroutine take_int_in

  subroutine take_dbl(y) bind(c)
    real(c_double), value :: y
    last_dbl = y
  end subroutine take_dbl
end module bindc_54_mod

program bindc_54
  use bindc_54_mod
  use iso_c_binding, only: c_int, c_double
  implicit none
  integer(c_int) :: a(3)
  real(c_double) :: b(2)

  a = [11, 22, 33]
  b = [1.5d0, 2.5d0]

  ! array element to a VALUE dummy (the original failing construct)
  call take_int(a(1))
  if (last_int /= 11) error stop

  call take_int(a(2))
  if (last_int /= 22) error stop

  ! array element with VALUE, INTENT(IN)
  call take_int_in(a(3))
  if (last_int /= 33) error stop

  ! expression involving an array element
  call take_int(a(2) + a(3))
  if (last_int /= 55) error stop

  ! real array element to a VALUE dummy
  call take_dbl(b(1))
  if (abs(last_dbl - 1.5d0) > 1.0d-12) error stop

  call take_dbl(b(2))
  if (abs(last_dbl - 2.5d0) > 1.0d-12) error stop

  print *, "ok"
end program bindc_54
