! The module is a separate translation unit, used only by a blanket use
! inside BLOCK. That import shadows the host dummy, so the dummy stays
! data. `data_one` is listed first: a real actual selects it, and a
! procedure actual selects `proc_one`.
program implicit_interface_120
implicit none
interface g
  procedure data_one
  procedure proc_one
end interface
call g(1.0)
call g(sq)
contains
real function sq(x)
  real :: x
  sq = x*x
end function
subroutine data_one(f)
  real :: f
  block
    use implicit_interface_120_m
    if (f(2.0) /= 2.0) error stop 1
  end block
  if (f /= 1.0) error stop 2
end subroutine
subroutine proc_one(f)
  real :: f
  if (f(3.0) /= 9.0) error stop 3
end subroutine
end program
