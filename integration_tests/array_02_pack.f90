program array_02_pack
  integer :: m(4), p(4)
  m = [ 1, 0, 0, 2 ]
  p = pack(m, m /= 0, [ 0, 0, 3, 4 ])
  print *, p
  if( any(p /= [1, 2, 3, 4]) ) error stop
end program
