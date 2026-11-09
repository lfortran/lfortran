! `data_one` is listed first. Its only `f(...)` is a block-local interface,
! including from a nested block, so the host dummy stays data. `proc_one`
! calls the dummy. A procedure actual must select `proc_one`. The external
! `f` is the interface procedure, not the dummy.
real function f(x)
  real :: x
  f = x
end function

program implicit_interface_118
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
    interface
      real function f(x)
        real :: x
      end function
    end interface
    block
      if (f(2.0) /= 2.0) error stop 1
    end block
  end block
  if (f /= 4.0) error stop 2
end subroutine
subroutine proc_one(f)
  real :: f
  if (f(3.0) /= 9.0) error stop 3
end subroutine
end program
