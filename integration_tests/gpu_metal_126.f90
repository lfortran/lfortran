! Test: multiple do concurrent loops calling the same module function
! should not produce duplicate function definitions in the Metal shader.
module gpu_metal_126_m
  implicit none
contains
  pure integer function f(x)
    integer, intent(in) :: x
    f = x + 1
  end function
end module

program gpu_metal_126
  use gpu_metal_126_m
  implicit none
  integer :: a(4), b(4), i

  do concurrent (i = 1:4)
    a(i) = f(i)
  end do

  do concurrent (i = 1:4)
    b(i) = f(i)
  end do

  if (a(1) /= 2) error stop
  if (a(2) /= 3) error stop
  if (a(3) /= 4) error stop
  if (a(4) /= 5) error stop
  if (b(1) /= 2) error stop
  if (b(2) /= 3) error stop
  if (b(3) /= 4) error stop
  if (b(4) /= 5) error stop
  print *, "ok"
end program
