! `data_one` is listed first and only calls a block-local `f`. `proc_one`
! calls the dummy. A procedure actual must select `proc_one`. The external
! `f` is the block-local call, not the dummy.
real function f(x)
  real :: x
  f = x
end function

program implicit_interface_117
implicit none
interface g
  procedure data_one
  procedure proc_one
end interface
call g(sq)
contains
real function sq(x)
  real :: x
  sq = x*x
end function
subroutine data_one(f)
  real :: f
  block
    real :: f
    if (f(2.0) /= 2.0) error stop 1
  end block
  if (f /= 4.0) error stop 2
end subroutine
subroutine proc_one(f)
  real :: f
  ! GFortran distinguishes this specific from data_one by a call in the
  ! procedure body, not inside BLOCK. The BLOCK call is the host dummy too.
  block
    real :: y
    if (f(3.0) /= 9.0) error stop 3
  end block
  if (f(3.0) /= 9.0) error stop 4
end subroutine
end program
