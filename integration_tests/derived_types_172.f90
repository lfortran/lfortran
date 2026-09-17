program derived_types_172
    implicit none

    type :: nested_t
        integer :: y
    end type

    type :: payload_t
        integer :: x
        integer :: arr(4)
        type(nested_t) :: child
    end type

    type :: scalar_holder_t
        integer, pointer :: p
    end type

    type :: array_holder_t
        integer, pointer :: p(:)
    end type

    type(payload_t), target :: obj
    type(scalar_holder_t) :: from_component, from_element, from_nested
    type(array_holder_t) :: from_section

    obj%x = 42
    obj%arr = [1, 2, 3, 4]
    obj%child%y = 7

    from_component = scalar_holder_t(obj%x)
    if (.not. associated(from_component%p, obj%x)) error stop "component target"
    if (from_component%p /= 42) error stop "component value"

    from_element = scalar_holder_t(obj%arr(3))
    if (.not. associated(from_element%p, obj%arr(3))) error stop "array element target"
    if (from_element%p /= 3) error stop "array element value"

    from_nested = scalar_holder_t(obj%child%y)
    if (.not. associated(from_nested%p, obj%child%y)) error stop "nested target"
    if (from_nested%p /= 7) error stop "nested value"

    from_section = array_holder_t(obj%arr(2:3))
    if (.not. associated(from_section%p)) error stop "array section target"
    if (size(from_section%p) /= 2) error stop "array section size"
    if (from_section%p(1) /= 2 .or. from_section%p(2) /= 3) error stop "array section value"
end program derived_types_172
