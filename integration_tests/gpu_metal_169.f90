program gpu_metal_169
  implicit none
  integer, parameter :: n(3) = [7, 42, 99]
  integer :: s(3)
  integer :: i

  do concurrent(i = 1:3)
    s(i) = n(i)
  end do

  print *, s(1), s(2), s(3)
  if (s(1) /= 7) error stop
  if (s(2) /= 42) error stop
  if (s(3) /= 99) error stop
end program
