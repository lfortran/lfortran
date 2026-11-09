! The only call named like the dummy is a block-local variable, which is an
! external function at the call. The host dummy stays data, so a real actual
! matches. The external `f` is what the block calls.
real function f(x)
  real :: x
  f = x
end function

program implicit_interface_116
implicit none
interface g
  procedure a
end interface
call g(1.0)
contains
subroutine a(f)
  real :: f
  block
    real :: f
    block
      real :: y
      if (f(2.0) /= 2.0) error stop 1
    end block
  end block
  if (f /= 1.0) error stop 2
end subroutine
end program
