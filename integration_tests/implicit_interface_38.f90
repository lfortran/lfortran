program implicit_interface_38
  implicit none

  call sub(*10, *20)
  error stop "alternate return did not jump"

10 continue
  stop 0

20 continue
  error stop "unexpected alternate return"

end program
