module gpu_metal_85_m
  implicit none
contains
  pure function compute(x) result(r)
    real, intent(in) :: x
    real, allocatable :: r(:)
    allocate(r(1))
    r(1) = x * 2.0
  end function
end module

program gpu_metal_85
  use gpu_metal_85_m
  implicit none
  real :: a(4), b(4)
  integer :: i

  b = [1.0, 2.0, 3.0, 4.0]

  do concurrent (i = 1:4)
    a(i:i) = compute(b(i))
  end do

  if (abs(a(1) -  2.0) > 1e-5) error stop
  if (abs(a(2) -  4.0) > 1e-5) error stop
  if (abs(a(3) -  6.0) > 1e-5) error stop
  if (abs(a(4) -  8.0) > 1e-5) error stop
  print *, "PASSED"
end program
