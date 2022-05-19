program array_01_pack
  integer :: m(6), p(2)
  m = [ 1, 0, 0, 0, 5, 0 ]
  p = pack(m, m /= 0)
end program