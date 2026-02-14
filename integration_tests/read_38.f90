program p
  integer :: a
  character(len=5) :: b
  character(len=20) :: s

  s = "42,hello"
  read(s, *) a, b

  if (a /= 42)error stop
  if (b /= "hello")error stop
end program
