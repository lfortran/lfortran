module class_c_reset_m
    implicit none
    type :: item
        integer :: value
    contains
        procedure :: noop
    end type
contains
    subroutine noop(x)
        class(item), intent(inout) :: x
    end subroutine
end module

program class_c_reset
    use class_c_reset_m
    implicit none
    integer :: x
    x = 5
    if (x /= 5) error stop 1
    x = x + 1
    if (x /= 6) error stop 2
end program
