module class_91_mod
    implicit none

    ! Base abstract type
    type, abstract :: toml_value
    end type toml_value

    ! Concrete extended type
    type, extends(toml_value) :: toml_integer
        integer :: value
    end type toml_integer

end module class_91_mod


program class_91
    use class_91_mod
    implicit none

    type(toml_integer), target :: i
    class(toml_value), pointer :: src
    class(toml_value), allocatable :: tmp

    ! Initialize concrete object
    i%value = 42

    ! Pointer must associate with extended type
    src => i
    if (.not. associated(src)) then
        error stop "ERROR: src pointer not associated"
    end if

    ! Allocate polymorphic allocatable from pointer source
    allocate(tmp, source=src)

    if (.not. allocated(tmp)) then
        error stop "ERROR: tmp not allocated"
    end if

    ! Validate dynamic type and value
    select type (tmp)
    type is (toml_integer)
        if (tmp%value /= 42) then
            error stop "ERROR: value mismatch after allocate(source=)"
        end if
        print *, "Copied value:", tmp%value
    class default
        error stop "ERROR: wrong dynamic type in tmp"
    end select

end program class_91
