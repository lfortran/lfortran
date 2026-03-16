module hashmap_wrappers_assign
    implicit none
    type :: key_type
        integer, allocatable :: value(:)
    end type key_type
end module hashmap_wrappers_assign

program allocatable_component_assign_01
    use hashmap_wrappers_assign
    implicit none

    type(key_type) :: key
    integer :: test(5) = [1, 2, 3, 4, 5]

    key%value = test
    if (size(key%value) /= 5) error stop
    if (key%value(1) /= 1) error stop
    if (key%value(5) /= 5) error stop
    if (.not. all(key%value == test)) error stop
end program allocatable_component_assign_01
