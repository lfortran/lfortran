! A program generic whose specific is an internal procedure. The dummy is
! declared only with a type and is a procedure because the body calls it.
program implicit_interface_114
implicit none
interface g
  procedure a
end interface
call g(sq)
contains
real function sq(x)
  real :: x
  sq = x*x
end function
subroutine a(f)
  real :: f
  if (f(3.0) /= 9.0) error stop
end subroutine
end program
