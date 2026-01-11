program old_charlen

  character*5 string5         ! Compiles with a constant
  character*(5) string5const  ! Constant with parens
  character*(2+3) string5expr ! Expression

  string5 = "hello"
  string5const = "world"
  string5expr = "12345"

  print *, string5
  print *, string5const
  print *, string5expr

  if (string5 /= "hello") error stop
  if (string5const /= "world") error stop
  if (string5expr /= "12345") error stop

end program
