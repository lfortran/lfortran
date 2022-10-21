program test_ichar
  integer :: i
  integer(8) :: li, li1
  i = ichar(' ')
  li = ichar('a', 8)
  li1 = ichar('b', kind=8)
  print *, i, li, li1
end program test_ichar
