program data_14
  integer, parameter :: n = 4
  real, dimension(n) :: arr
  data (arr(i), integer :: i = 1,n) /0.0, 0.0, 0.0, 0.0/
  print *, arr
end program
