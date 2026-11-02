! A host whose internal procedure is an actual argument has a finalizable
! local and a local whose bounds inquire about it: it is declared in the
! same scope as the finalizable local, which is finalized when the host
! ends. An inquiry about a local of the same procedure in a specification
! expression is an extension (F2018 10.1.11), which GFortran evaluates
! differently, so this test is not run with GFortran.
module finalization_55_mod
implicit none
integer :: finalized = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x(:)
    finalized = finalized + 1
end subroutine
subroutine apply(f, r)
    interface
        integer function f()
        end function
    end interface
    integer, intent(out) :: r
    r = f()
end subroutine
recursive subroutine host(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    type(t) :: x(n)
    integer :: w(size(x, 1) + 1)
    w = 1
    call apply(get, r)
contains
    integer function get()
        get = sum(w)
    end function
end subroutine
end module

program finalization_55
use finalization_55_mod
implicit none
integer :: r
call host(3, r)
print *, r, finalized
if (r /= 4) error stop
if (finalized /= 1) error stop
end program
