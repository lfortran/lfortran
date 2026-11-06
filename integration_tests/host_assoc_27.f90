! A procedure value of an internal procedure with an intent(out) dummy of a
! finalizable type: the dummy is finalized once on entry, by the internal
! procedure, not again by the thunk that calls it
module host_assoc_27_m
implicit none
integer :: finalized = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
abstract interface
    subroutine setter(x)
        import :: t
        type(t), intent(out) :: x
    end subroutine
end interface
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    finalized = finalized + 1
end subroutine

subroutine call_setter(f, x)
    procedure(setter) :: f
    type(t), intent(inout) :: x
    call f(x)
end subroutine

subroutine host(n)
    integer, intent(in) :: n
    type(t) :: x
    x%v = -1
    finalized = 0
    call call_setter(set, x)
    if (finalized /= 1) error stop 1
    if (x%v /= n) error stop 2
contains
    subroutine set(y)
        type(t), intent(out) :: y
        y%v = n
    end subroutine
end subroutine
end module host_assoc_27_m

program host_assoc_27
use host_assoc_27_m
implicit none
call host(7)
print *, "ok"
end program host_assoc_27
