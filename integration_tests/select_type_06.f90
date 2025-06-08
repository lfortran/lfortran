program select_type_06_m
    implicit none

    type :: string_value
        character(len=:), allocatable :: raw
    end type

    type, extends(string_value) :: toml_keyval
        character(len=20) :: key = "example"
    end type

    class(string_value), allocatable :: val  ! Polymorphic variable

    allocate(val)
    select type(val)
    type is(string_value)
        val%raw = "Hello, Fortran!"
    end select

    if (val%raw /= "Hello, Fortran!") error stop

    deallocate(val)
    allocate(toml_keyval :: val)
  
    select type(val)
    class is(toml_keyval)
        if (val%key /= "example") error stop
    end select
end program
