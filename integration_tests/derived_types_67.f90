module derived_types_67_type_mod
    implicit none
    private
    public :: my_type, my_val

    type :: my_val
    end type

    type, extends(my_val) :: my_type
        integer :: x
    end type
end module derived_types_67_type_mod

module derived_types_67_cast_utils
    use derived_types_67_type_mod, only: my_type, my_val
    implicit none
    private
    public :: cast_to_my_type, my_type, my_val

contains

    function cast_to_my_type(obj) result(p)
        class(my_val), intent(in), target :: obj
        type(my_type), pointer :: p
        select type(obj)
            class is(my_type)
                p => obj
        end select
    end function cast_to_my_type

end module derived_types_67_cast_utils


program derived_types_67
    use derived_types_67_cast_utils, only: temp_my_type => my_type, my_val, cast_to_my_type
    implicit none
    class(my_val), allocatable :: j_value
    type(temp_my_type), pointer :: ptr
    allocate(temp_my_type :: j_value)

    select type(j_value)
        class is(temp_my_type)
            j_value%x = 42
    end select
    ptr => cast_to_my_type(j_value)
    if (ptr%x /= 42) error stop
end program derived_types_67
