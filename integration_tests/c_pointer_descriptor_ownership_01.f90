module c_pointer_descriptor_ownership_01_m
    implicit none
    type :: base
        integer, pointer :: items(:) => null()
    end type
    type, extends(base) :: child
        integer, pointer :: other(:) => null()
    end type
    type :: box
        type(child) :: nested
    end type
end module

program c_pointer_descriptor_ownership_01
    use c_pointer_descriptor_ownership_01_m
    implicit none
    type(box) :: first, second
    integer, target :: values(-2:2), alternate(3)

    values(-2) = 11
    values(2) = 29
    alternate(1) = 31
    alternate(3) = 37
    if (associated(first%nested%items)) error stop 1
    first%nested%items => values
    second%nested%items => values
    if (.not. associated(first%nested%items, values)) error stop 2
    if (first%nested%items(-2) /= 11) error stop 3
    if (second%nested%items(2) /= 29) error stop 4
    first%nested%other => second%nested%items
    first%nested%items => alternate
    if (first%nested%items(3) /= 37) error stop 5
    if (.not. associated(first%nested%other, values)) error stop 6
    second%nested%items => null()
    if (associated(second%nested%items)) error stop 7
    if (first%nested%other(-2) /= 11) error stop 8
    if (first%nested%items(1) /= 31) error stop 9
    first%nested%items => null()
    if (associated(first%nested%items)) error stop 10
    if (first%nested%other(2) /= 29) error stop 11
end program
