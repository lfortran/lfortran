module procedure_60_mod
implicit none
integer :: e_calls = 0
integer :: g_calls = 0
end module

program procedure_60
use procedure_60_mod, only: e_calls, g_calls
implicit none
abstract interface
    subroutine s()
    end subroutine
end interface
type :: t
    procedure(s), pointer, nopass :: f
end type
procedure(s) :: e, g
type(t), parameter :: x = t(e)
type(t) :: y = t(g)
call x%f()
if (e_calls /= 1 .or. g_calls /= 0) error stop
call y%f()
call y%f()
if (e_calls /= 1 .or. g_calls /= 2) error stop
y%f => e
call y%f()
if (e_calls /= 2 .or. g_calls /= 2) error stop
print *, e_calls, g_calls
end program

subroutine e()
use procedure_60_mod, only: e_calls
implicit none
e_calls = e_calls + 1
end subroutine

subroutine g()
use procedure_60_mod, only: g_calls
implicit none
g_calls = g_calls + 1
end subroutine
