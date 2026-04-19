! Test: passing allocatable component of derived type array element to a
! function inside do concurrent. Exercises correct Metal codegen for
! struct-member array arguments (no address-of-rvalue) and runtime-sized
! pre-allocation of output struct members.
program gpu_metal_193
  implicit none

  type :: vec_t
    real, allocatable :: v(:)
  end type

  integer, parameter :: n = 4
  type(vec_t) :: a(n), b(n)
  integer :: i

  do i = 1, n
    allocate(a(i)%v(1))
    a(i)%v(1) = real(i)
  end do

  do concurrent(i = 1:n)
    b(i) = f(a(i)%v)
  end do

  if (abs(b(1)%v(1) - 1.0) > 1e-6) error stop
  if (abs(b(2)%v(1) - 2.0) > 1e-6) error stop
  if (abs(b(3)%v(1) - 3.0) > 1e-6) error stop
  if (abs(b(4)%v(1) - 4.0) > 1e-6) error stop
  print *, "PASS"

contains
  pure function f(x) result(r)
    real, intent(in) :: x(:)
    type(vec_t) :: r
    r%v = x
  end function
end program
