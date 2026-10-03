! Procedures with character results for implicit_interface_84.f90.
character(len=8) function ii84_up8(s)
  character(len=*) :: s
  ii84_up8 = s
end function

character(len=5) function ii84_c1(i)
  integer :: i
  write(ii84_c1, '(a,i1,i1)') "one", i, i
end function

character(len=5) function ii84_c2(i)
  integer :: i
  write(ii84_c2, '(a,i1,i1)') "two", i, i
end function
