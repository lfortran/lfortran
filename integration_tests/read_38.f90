program p
  integer :: a
  character(len=5) :: b
  character(len=20) :: s

  s = "42,hello"
  read(s, *) a, b

  print* , a, b
end program
