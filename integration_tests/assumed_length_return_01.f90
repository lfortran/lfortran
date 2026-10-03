character(len=*) function make_token()
  make_token = "A"
end function make_token

subroutine demo(n)
  integer, intent(in) :: n
  character(len=n), external :: make_token
  character(len=n) :: res

  res = make_token()
  if (res /= "A  ") error stop
end subroutine demo

program assumed_length_return_01
  call demo(3)
end program assumed_length_return_01
