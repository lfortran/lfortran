program implicit_interface_34
  implicit none

  character*2 foo, s1, s2, s3

  s1 = 'too long'
  s2 = 'also long'
  s3 = s1 // foo (s2)
  
  print *, 's3 = ', s3, merge (' : pass', ' : fail', s3 == 'to')
  if(s3 /="to") error stop
  if(merge (' : pass', ' : fail', s3 == 'to') /=" : pass") error stop

end program

function foo (s)
  character(2) :: foo
  character(*) :: s

  foo = s

end function