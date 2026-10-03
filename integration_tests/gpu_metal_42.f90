! Test: do concurrent body references a local VLA whose size depends
! on a subroutine argument not otherwise used in the body/head.
! The gpu_offload pass must collect the argument from the variable's
! type expression into the kernel parameter list.
program gpu_metal_42
implicit none
type :: dims
  integer :: n
  integer :: m
end type
type(dims) :: d1, d2
d1%n = 4
d2%m = 4
call test(d1, d2)
contains
subroutine test(x, y)
  type(dims), intent(in) :: x, y
  integer :: i
  ! 'y' is ONLY referenced in the VLA dimension, not in body or head
  real :: a(y%m)
  do concurrent(i = 1:x%n)
    a(i) = real(i)
  end do
  if (abs(a(1) - 1.0) > 1.0e-6) error stop
  if (abs(a(2) - 2.0) > 1.0e-6) error stop
  if (abs(a(3) - 3.0) > 1.0e-6) error stop
  if (abs(a(4) - 4.0) > 1.0e-6) error stop
  print *, "ok"
end subroutine
end program
