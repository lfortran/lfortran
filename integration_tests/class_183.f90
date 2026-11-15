module class_type_only_m
    implicit none
    type :: item
        integer :: value
    end type
contains
    subroutine unused(x)
        class(item), intent(inout) :: x
    end subroutine
end module

program class_type_only
    use class_type_only_m
    implicit none
    type(item) :: x
    x%value = 5
    if (x%value /= 5) error stop 1
end program
