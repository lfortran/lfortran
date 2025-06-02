program format_33  
  implicit none ! file backspace.f90
  character(20):: string
  open(42,status='scratch')
  write(42,"(A)") 'Hello world'
  backspace 42
  read(42,"(A)") string
  print "(A)", string
  if (string /= 'Hello world') error stop
end program format_33
