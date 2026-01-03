program array_01_pack
  integer :: m(6), p(2)
  integer :: s(1)
  m = [ 1, 0, 0, 0, 5, 0 ]
  p = pack(m, m /= 0)
  s = shape(pack(m, m /= 0))
  print *, s(1)
  if (s(1) /= 2) error stop
end program
