module class_pdt_reset_m
    implicit none
    type :: base(k)
        integer, kind :: k = 4
        integer :: parent_value = 17
    end type
    type, extends(base) :: child
        integer :: child_value = 42
    end type
contains
    subroutine reset(x)
        class(child), intent(out) :: x
        x%parent_value = 19
        x%child_value = 43
    end subroutine
end module

program class_pdt_reset
    use class_pdt_reset_m
    implicit none
    type(child) :: x
    if (x%k /= 4) error stop 2
    x%parent_value = 99
    x%child_value = 99
    call reset(x)
    if (x%parent_value /= 19 .or. x%child_value /= 43) error stop 1
end program
