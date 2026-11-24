! The parent's interface assignment(=) has type(st) dummies. An extended
! type is not compatible with those dummies, and `use ..., only: st` does
! not bring the generic into scope, so this is an intrinsic assignment.
module assign_extend_01_m
    implicit none
    type :: st
        integer :: state = 0
    end type
    interface assignment(=)
        module procedure st_assign
    end interface
contains
    subroutine st_assign(to, from)
        type(st), intent(inout) :: to
        type(st), intent(in) :: from
        to%state = from%state + 100
    end subroutine
end module

program assign_extend_01
    use assign_extend_01_m, only: st
    implicit none
    type, extends(st) :: st2
    end type
    type(st2) :: a, b
    a%state = 5
    b = a
    print *, b%state
    if (b%state /= 5) error stop 1
end program
