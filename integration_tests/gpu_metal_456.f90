module gpu_metal_456_m
implicit none
type :: tensor_t
  real, allocatable :: values_(:)
contains
  procedure :: values
end type
contains
pure function values(self) result(r)
  class(tensor_t), intent(in) :: self
  real, allocatable :: r(:)
  r = self%values_
end function
end module

program gpu_metal_456
! An allocatable local of a BLOCK in an offloaded loop, assigned the
! result of a function, has the shape the function gives its result:
! here the size of a component of the iteration's element, which the
! host works out for every iteration before the launch.
use gpu_metal_456_m
implicit none
integer, parameter :: n = 4, np = 3
type(tensor_t) :: x(np)
real :: a(n, np)
integer :: p, i
do p = 1, np
  x(p)%values_ = [(real(10*p + i), i = 1, p + 1)]
end do
a = 0
do concurrent (p = 1:np)
  block
    real, allocatable :: t(:)
    t = x(p)%values()
    a(1:size(t), p) = 2 * t
  end block
end do
do p = 1, np
  do i = 1, n
    if (i <= p + 1) then
      if (abs(a(i, p) - 2 * real(10*p + i)) > 1e-5) error stop
    else
      if (a(i, p) /= 0) error stop
    end if
  end do
end do
print *, a
print *, "PASSED"
end program
