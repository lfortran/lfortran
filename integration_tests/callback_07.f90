! C calls an external Fortran subroutine without bind(c) whose EXTERNAL
! dummy procedures have implicit interfaces, passing raw C function pointers.
! A Fortran procedure value is a plain code pointer, so the C side uses the
! GFortran calling convention: arguments by reference, external names with a
! trailing underscore.
subroutine callback_07_apply(f, g, x, r)
implicit none
external :: f
integer, external :: g
integer, intent(in) :: x
integer, intent(out) :: r
integer :: t
call f(x, t)
call f(t, r)
r = r + g(x)
end subroutine

program callback_07
implicit none
interface
    subroutine callback_07_driver(status)
    integer, intent(out) :: status
    end subroutine
end interface
integer :: status
status = -1
call callback_07_driver(status)
print *, status
if (status /= 0) error stop
end program
