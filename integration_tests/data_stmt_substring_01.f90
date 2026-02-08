program data_stmt_substring_01
  implicit none

  character(6) :: s1
  character(10) :: s2 = 'XXXXXXXXXX'
  data s1(1:6) / 'hello!'/
  data s2(2:5) / 'test'/

  if (s1 /= 'hello!') error stop
  if (s2 /= 'XtestXXXXX') error stop

end program
