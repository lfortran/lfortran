! An array constructor with a type-spec whose elements are structure
! constructors of a type with an allocatable component of a finalizable
! type, passed as an actual argument (#3140). The number of finalizations
! of the temporaries is not checked.
module finalization_59_mod
    implicit none
    type finalizable
        integer :: i
    contains
        final :: print_finalized
    end type
    type t
        type(finalizable), allocatable :: x
    end type
    integer :: sum_seen = 0
    integer :: calls = 0
contains
    subroutine sub(x)
        type(t), intent(in) :: x(2)
        print *, "in sub"
        calls = calls + 1
        if (.not. allocated(x(1)%x)) error stop 1
        if (.not. allocated(x(2)%x)) error stop 2
        if (x(1)%x%i /= 1) error stop 3
        if (x(2)%x%i /= 2) error stop 4
        sum_seen = x(1)%x%i + x(2)%x%i
    end subroutine
    subroutine print_finalized(x)
        type(finalizable), intent(inout) :: x
        print *, "finalizing t(finalizable)", x%i
    end subroutine
end module

program finalization_59
    use finalization_59_mod
    implicit none
    call sub([t:: t(x=finalizable(1)), t(x=finalizable(2))])
    if (calls /= 1) error stop 5
    if (sum_seen /= 3) error stop 6
end program
