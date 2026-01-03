program nullify_07
  implicit none
  character(len=:), pointer :: str
  allocate(character(len=5) :: str)
  str = "Hello"
  print *, str
  nullify(str)
  if(len(str) /= 0) error stop
end program nullify_07