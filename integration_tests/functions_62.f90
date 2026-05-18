program functions_62
  implicit none
  integer get_count
  if(get_count() /= 1) stop "get_count() should return 1 "
  print *, "test passed"
end program functions_62

integer function get_count()
  integer cnt
  save
  data cnt /0/
  cnt = cnt + 1
  get_count = cnt
end function get_count