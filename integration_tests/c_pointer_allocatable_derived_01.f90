module c_pointer_allocatable_derived_01_m
    implicit none
    type :: cell
        integer :: value
    end type
    type :: wrapper
        type(cell) :: direct
        type(cell), allocatable :: component
    end type
contains
    subroutine check_owner(object)
        type(cell), allocatable, target, intent(inout) :: object
        type(cell), pointer :: alias
        alias => object
        if (.not. associated(alias, object)) error stop 1
        alias%value = alias%value + 1
        alias => null()
    end subroutine
end module

program c_pointer_allocatable_derived_01
    use c_pointer_allocatable_derived_01_m
    implicit none
    type(cell), allocatable, target :: object, other
    type(wrapper), target :: box
    type(cell), target :: plain
    type(cell), pointer :: alias, pointer_target

    allocate(object, other, box%component)
    object%value = 7
    alias => object
    if (.not. associated(alias, object)) error stop 2
    if (associated(alias, other)) error stop 3
    alias%value = 9
    if (object%value /= 9) error stop 4

    alias => box%component
    if (.not. associated(alias, box%component)) error stop 5
    alias%value = 12
    if (box%component%value /= 12) error stop 6

    alias => box%direct
    if (.not. associated(alias, box%direct)) error stop 7
    alias%value = 15
    if (box%direct%value /= 15) error stop 8

    alias => plain
    pointer_target => alias
    if (.not. associated(pointer_target, alias)) error stop 9
    pointer_target%value = 21
    if (plain%value /= 21) error stop 10

    alias => null()
    pointer_target => null()
    call check_owner(object)
    if (object%value /= 10) error stop 11
    deallocate(object, other, box%component)
end program
