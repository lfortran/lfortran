module class_intent_out_inherited_c_m
    implicit none
    type :: base
        integer :: value = 17
        integer :: values(0:1) = [5, 8]
    end type
    type, extends(base) :: middle
        integer :: extra = 42
    end type
    type, extends(middle) :: child
        integer :: more = 71
    end type
contains
    subroutine unused(x)
        class(base), intent(out) :: x
    end subroutine
end module

program class_intent_out_inherited_c
    use class_intent_out_inherited_c_m, only: renamed_child => child
    implicit none
    type(renamed_child) :: x
    x%value = 19
    x%values(0) = 11
    x%values(1) = 12
    x%extra = 46
    x%more = 73
    if (x%value /= 19 .or. x%values(0) /= 11 .or. &
            x%values(1) /= 12) error stop 1
    if (x%middle%base%value /= 19) error stop 2
    if (x%extra /= 46 .or. x%more /= 73) error stop 3
end program
