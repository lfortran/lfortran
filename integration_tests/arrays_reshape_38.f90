program arrays_reshape_38
  implicit none
  integer(8) :: n
  integer(8), dimension(:), allocatable :: x
  integer(8), allocatable :: x_tmp(:)

  x = [1_8, 2_8, 3_8, 4_8, 5_8]
  n = size(x, kind=8)
  if (n /= 5) error stop
  x_tmp = reshape(x, [n])
  if (size(x_tmp) /= 5) error stop
  if (x_tmp(1) /= 1) error stop
  if (x_tmp(5) /= 5) error stop
end program
