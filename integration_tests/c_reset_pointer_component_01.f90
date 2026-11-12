module c_reset_pointer_component_01_m
    implicit none
    integer, target, save :: default_target = 17
    integer, target, save :: default_numbers(2) = [7, 9]
    type :: item
        integer, pointer :: value => default_target
        integer, pointer :: items(:) => null()
    end type
contains
    subroutine unused(x)
        class(item), intent(inout) :: x
    end subroutine
end module

program c_reset_pointer_component_01
    use c_reset_pointer_component_01_m
    implicit none
    type(item) :: x, other_pointer
    integer, target :: other = 12, numbers(2) = [3, 5]
    x%value => other
    x%value = 44
    if (other /= 44) error stop 1
    x%value => default_target
    x%value = 21
    if (default_target /= 21) error stop 2
    x%value => null()
    if (associated(x%value)) error stop 3
    x%items => numbers
    x%items(2) = 13
    if (numbers(2) /= 13) error stop 4
    if (.not. associated(x%items, numbers)) error stop 5
    other_pointer%items => numbers
    x%items => other_pointer%items
    other_pointer%items => null()
    if (.not. associated(x%items, numbers)) error stop 7
    x%items => default_numbers
    if (x%items(1) /= 7) error stop 8
    x%items(2) = 23
    if (default_numbers(2) /= 23) error stop 9
    x%items => null()
    if (associated(x%items)) error stop 6
end program
