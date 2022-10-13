program array_01_transfer
  integer :: m(6), size = 2
  m = [ 1, 0, 0, 0, 5, 0 ]
  print *, transfer(m, 1.0, size)
end program