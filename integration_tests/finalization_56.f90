! A host with a finalizable local calls through a procedure pointer whose
! interface imports a variable of the host, so it points its single frame slot
! at its frame and restores the slot when it ends, also when it returns
! early: the local is finalized once on every path.
module finalization_56_mod
implicit none
integer :: finalized = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    finalized = finalized + 1
end subroutine
subroutine host(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    type(t) :: x
    integer :: b(n)
    interface
        subroutine inner(a)
            import :: n
            integer, intent(inout) :: a(n)
        end subroutine
    end interface
    procedure(inner), pointer :: p
    b = 1
    p => add2
    call p(b)
    s = sum(b)
    if (s > 10) return
    x%v = s
contains
    subroutine add2(a)
        integer, intent(inout) :: a(n)
        a = a + 2
    end subroutine
end subroutine
end module

program finalization_56
use finalization_56_mod
implicit none
integer :: s
call host(3, s)
print *, s, finalized
if (s /= 9) error stop
if (finalized /= 1) error stop
call host(4, s)
print *, s, finalized
if (s /= 12) error stop
if (finalized /= 2) error stop
end program
