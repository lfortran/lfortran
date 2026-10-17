module gpu_metal_453_m
implicit none
type :: tensor_t
  real, allocatable :: values_(:)
contains
  procedure :: values
end type
type :: point_t
  real :: x
end type
contains
pure function values(self) result(r)
  class(tensor_t), intent(in) :: self
  real, allocatable :: r(:)
  r = self%values_
end function
pure function points(p, n) result(r)
  integer, intent(in) :: p, n
  type(point_t) :: r(n)
  integer :: i
  do i = 1, n
    r(i)%x = real(p*i)
  end do
end function
pure function twice(x) result(r)
  real, intent(in) :: x(:)
  real :: r(size(x))
  r = 2*x
end function
end module

program gpu_metal_453
! A call of a function returning an array, in an array expression of an
! offloaded loop, is made once before the statement, not once for every
! element the statement computes: assigned to a section, in an elemental
! expression, reduced by sum, and as an operand of matmul. A result of a
! derived type is still read element by element.
use gpu_metal_453_m
implicit none
integer, parameter :: n = 3, np = 2
type(tensor_t) :: x(np)
real :: a(n, np), b(n, np), e(n, np), c(np), w(n, n), z(n, np)
type(point_t) :: q(n, np)
integer :: p, i
do p = 1, np
  x(p)%values_ = [real(p), real(2*p), real(3*p)]
end do
do i = 1, n
  w(i, :) = real(i)
end do
b = 1
do concurrent (p = 1:np)
  a(1:n, p) = x(p)%values()
  e(1:n, p) = b(1:n, p) + x(p)%values() * 2
  c(p) = sum(x(p)%values() * b(1:n, p))
  z(1:n, p) = matmul(w, twice(a(1:n, p)))
  q(1:n, p) = points(p, 3)
end do
print *, a, e, c, z
do p = 1, np
  do i = 1, n
    if (abs(a(i, p) - real(i*p)) > 1e-5) error stop
    if (abs(e(i, p) - (1 + 2*real(i*p))) > 1e-5) error stop
    if (abs(z(i, p) - real(i) * 2 * real(6*p)) > 1e-4) error stop
    if (abs(q(i, p)%x - real(i*p)) > 1e-5) error stop
  end do
  if (abs(c(p) - real(6*p)) > 1e-5) error stop
end do
print *, "PASSED"
end program
