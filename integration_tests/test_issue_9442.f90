program readarray
  implicit none
  character(12):: clist = '1.0 42. -42.'
  real list(3)
  read(clist,*) list
  print "(3F6.1)",list
end program readarray
