! Test: sum() of allocatable function result inside do concurrent, where the
! function returns an allocatable array via implicit reallocation (r = a).
! This tests that the Metal backend correctly determines the size of
! allocatable arrays assigned from function parameters.
program gpu_metal_133
  implicit none
  integer :: i
  real :: x(2), y(1)
  x = [1.0, 2.0]
  do concurrent (i = 1:1)
    y(i) = sum(f(x))
  end do
  if (abs(y(1) - 3.0) > 1e-5) error stop
  print *, y(1)
contains
  pure function f(a) result(r)
    real, intent(in) :: a(:)
    real, allocatable :: r(:)
    r = a
  end function
end program
