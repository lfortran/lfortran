module modd_arrays_05_size
    integer(8),parameter :: arr_size = 10
end module modd_arrays_05_size

program arrays_05_size
  use modd_arrays_05_size
  integer :: arr(arr_size)
  print * , kind(arr_size)
  if (kind(arr_size) /= 8) error stop
  print * , size(arr)
  if (size(arr) /= 10) error stop
end program