character(8) function make_name()
  make_name = "abcdefgh"
end function make_name

program p
  character(4) :: out, make_name

  out = make_name()
  if (out /= "abcd") error stop
  print *, "ok"
end program p
