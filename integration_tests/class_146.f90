program class_146
    implicit none

    type :: string_value
        integer :: raw
    end type

    type, extends(string_value) :: toml_keyval
        character(len=24) :: key
    end type

    class(string_value), allocatable :: val

    allocate(toml_keyval :: val)
    deallocate(val)

end program
