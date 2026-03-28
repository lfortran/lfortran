program implicit_interface_36
  implicit none

  external :: x
  real :: x

  integer :: i
  real :: a(10)

  a = [(i + 0.5, i = 1, 10)]

  call sub_assumed(x, a)
  call sub_fixed(x, a)

end program implicit_interface_36

subroutine sub_assumed(F, Y)

  real F, FB
  real Y(*)   ! Previously caused ICE

  FB = F(Y)

  if (abs(FB - 60.0) > 0.0001) then
    error stop "assumed-size array argument failed"
  end if

end subroutine

subroutine sub_fixed(F, Y)

  real F, FB
  real Y(10)
 
  FB = F(Y)

  if (abs(FB - 60.0) > 0.0001) then
    error stop "fixed-size array argument failed"
  end if

end subroutine

real function x(a)
  real :: a(10)

  x = sum(a)

end function