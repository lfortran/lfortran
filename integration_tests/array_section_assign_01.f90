program array_section_assign_01
  implicit none
  integer :: ilist(6,6)
  ilist(1,1:3) = [1,2,4]
  print "(3(1X,I0))", ilist(1,1:3)
  
  if (any(ilist(1,1:3) /= [1, 2, 4])) error stop
end program array_section_assign_01