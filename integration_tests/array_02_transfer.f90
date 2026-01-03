program array_02_transfer
  integer :: m(7)
  m = [ 1, 0, 0, 0, 5, 0, 0 ]
  print *, transfer(m, [ 1.0 ])
end program