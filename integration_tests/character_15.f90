program character_15
  implicit none

  character*(*) hello
  parameter (hello = 'Hello world!')

  print *, hello
  if (len(hello) /= 12) error stop

end program character_15
