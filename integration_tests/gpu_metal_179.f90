! Test: user-defined function named 'or' (C++ alternative operator token)
! must be renamed in Metal shader to avoid keyword clash.
module gpu_metal_179_m
  implicit none
contains
  pure real function or(x)
    real, intent(in) :: x
    or = x
  end function
end module

program gpu_metal_179
  use gpu_metal_179_m
  implicit none
  real :: a(4), b(4)
  integer :: i
  a = [1.0, 2.0, 3.0, 4.0]
  do concurrent(i=1:4)
    b(i) = or(a(i))
  end do
  if (any(abs(b - a) > 1e-6)) error stop "FAIL"
  print *, "PASS"
end program
