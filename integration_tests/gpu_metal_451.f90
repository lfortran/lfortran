module gpu_metal_451_m
implicit none
contains
pure function rev(x) result(r)
  real, intent(in) :: x(:)
  real :: r(size(x))
  integer :: i
  do i = 1, size(x)
    r(i) = x(size(x) + 1 - i)
  end do
end function
end module

program gpu_metal_451
! A function that is not elemental, assigned to a whole array in an
! offloaded loop, is called with its array argument whole, not once per
! element with an element of it.
use gpu_metal_451_m
implicit none
real :: a(3, 2), b(3, 2)
integer :: p
a = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
do concurrent (p = 1:2)
  block
    real :: v(3)
    v = rev(a(1:3, p))
    b(:, p) = v
  end block
end do
print *, b
if (any(b(:, 1) /= [3., 2., 1.])) error stop
if (any(b(:, 2) /= [6., 5., 4.])) error stop
print *, "PASSED"
end program
