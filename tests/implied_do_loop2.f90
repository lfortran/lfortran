program implied_do_loop2
  implicit none
  character(len=5) :: s = "Hello"
  integer :: i
  print *, (s, i = 1, 3)
end program implied_do_loop2
