module gpu_metal_452_m
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

program gpu_metal_452
! An associate name bound to the result of a function, in an offloaded
! loop, is that result, computed once on entry, wherever it is read.
use gpu_metal_452_m
implicit none
integer, parameter :: n = 3, np = 2
type(tensor_t) :: outs(np)
real :: a(n, np), d(n, np), c(np)
integer :: p
do p = 1, np
  outs(p)%values_ = [real(p), real(2*p), real(3*p)]
end do
a = 1
do concurrent (p = 1:np)
  associate (y => outs(p)%values())
    c(p) = sum((y(1:n) - a(1:n, p))**2)
    d(1:n, p) = (a(1:n, p) - y(1:n)) * 2
  end associate
end do
print *, c, d
if (abs(c(1) - 5) > 1e-4) error stop
if (abs(c(2) - 35) > 1e-4) error stop
if (any(abs(d(:, 2) - [-2., -6., -10.]) > 1e-4)) error stop
print *, "PASSED"
end program
