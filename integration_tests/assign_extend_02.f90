! The generic is in scope, but its dummies are type(st). The extended
! variables are not compatible with those dummies, so the assignment
! stays intrinsic and must not call st_assign.
module assign_extend_02_m
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

program assign_extend_02
    use assign_extend_02_m
    implicit none
    type, extends(st) :: st2
    end type
    type(st2) :: a, b
    a%state = 5
    b = a
    print *, b%state
    if (b%state /= 5) error stop 1
end program
