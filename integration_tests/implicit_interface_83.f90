! A dummy procedure with an implicit interface and a character result is
! referenced as a function, including a result whose length the caller
! gives by an expression.
subroutine ii83_s(f)
  implicit none
  character(len=5), external :: f
  if (f(1) /= "hello") error stop 1
end subroutine

subroutine ii83_t(f)
  character(len=5) :: f
  external f
  character(len=5) :: s
  s = f(2)
  if (s /= "hello") error stop 2
end subroutine

subroutine ii83_u(n)
  implicit none
  integer, intent(in) :: n
  character(len=n), external :: ii83_tok
  character(len=n) :: res
  res = ii83_tok()
  if (res /= "A  ") error stop 3
  if (ii83_tok() /= "A  ") error stop 4
  if (len(ii83_tok()) /= n) error stop 5
end subroutine

program implicit_interface_83
  implicit none
  character(len=5), external :: ii83_cf
  call ii83_s(ii83_cf)
  call ii83_t(ii83_cf)
  call ii83_u(3)
  print *, "ok"
end program

character(len=5) function ii83_cf(i)
  integer :: i
  ii83_cf = "hello"
end function

character(len=*) function ii83_tok()
  ii83_tok = "A"
end function
