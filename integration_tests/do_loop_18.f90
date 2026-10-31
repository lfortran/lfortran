! The program of #16136: a completed loop leaves its DO variable one step
! past the end.
program do_loop_18
  integer :: i
  do i = 1,2
     print *,i
  end do
  print *,i
  if (i /= 3) error stop
end program
