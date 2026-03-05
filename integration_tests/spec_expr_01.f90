module spec_expr_01_mod
contains
  pure integer function nlen(n)
    integer, intent(in) :: n
    nlen = int(log10(real(abs(n), kind(1d0)) + 0.1d0)) + 1
  end function nlen
end module spec_expr_01_mod

program spec_expr_01
  use spec_expr_01_mod, only: nlen
  implicit none
  character(22) :: cinput = '42 666 -42 -666 10 9 0'
  integer :: input(7), i
  read(cinput, *) input
  if (input(1) /= 42) error stop
  if (input(2) /= 666) error stop
  if (input(3) /= -42) error stop
  if (input(4) /= -666) error stop
  if (input(5) /= 10) error stop
  if (input(6) /= 9) error stop
  if (input(7) /= 0) error stop
  do i = 1, 7
    print *, 'input was "'//i0(input(i))//'"'
  end do
contains
  pure function i0(n) result(outstring)
    integer, intent(in) :: n
    character(nlen(n)+1) :: outstring
    write(outstring, "(I0)") n
  end function i0
end program spec_expr_01
