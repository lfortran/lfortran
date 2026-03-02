program format_33  
  implicit none ! file backspace.f90
  character(20):: string
  integer :: unit_no
  open(newunit=unit_no, status='scratch')
  write(unit_no, "(A)") 'Hello world'
  backspace unit_no
  read(unit_no, "(A)") string
  print "(A)", string
  if (string /= 'Hello world') error stop
end program format_33
