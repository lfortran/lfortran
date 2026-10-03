program gpu_metal_124
  ! Test: pure function with assumed-shape array argument and
  ! runtime-sized result (size(x)) called via a runtime-size
  ! slice inside do concurrent. Exercises VLA workspace buffers
  ! for allocatable temporaries in Metal GPU codegen.
  implicit none
  integer :: i, k
  real :: z(3), a(3)

  k = 2
  z = 1.0
  a = 0.0

  do concurrent (i = 1:1)
    a(1:k) = f(z(1:k))
  end do

  if (abs(a(1) - 1.0) > 1e-5) error stop
  if (abs(a(2) - 1.0) > 1e-5) error stop
  if (a(3) /= 0.0) error stop
  print *, "PASSED"
contains
  pure function f(x) result(y)
    real, intent(in) :: x(:)
    real :: y(size(x))
    y = x
  end function
end program
