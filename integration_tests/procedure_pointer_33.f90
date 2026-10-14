! A dispatch table whose declarations are circular: the procedure pointer
! component's interface is the module procedure `a` defined later, the named
! constant `table` holds `a`, and the extent of `a`'s dummy argument is
! `size(table)`.
module procedure_pointer_33_mod
implicit none
type :: pp
    procedure(a), pointer, nopass :: pf
end type
type(pp), parameter :: table(1) = [pp(a)]
integer :: calls = 0
integer :: expected = 3
contains
    recursive subroutine a(t, n)
        type(pp), intent(in) :: t(size(table))
        integer, intent(in) :: n
        calls = calls + 1
        if (n /= expected) error stop
        expected = expected - 1
        if (n > 0) call t(1)%pf(t, n - 1)
    end subroutine
end module

program procedure_pointer_33
use procedure_pointer_33_mod
implicit none
call table(1)%pf(table, 3)
if (calls /= 4) error stop
if (expected /= -1) error stop
print *, calls
end program
