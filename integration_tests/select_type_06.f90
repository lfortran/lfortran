program select_type_06_m
    implicit none

    type :: string_value
        character(len=:), allocatable :: raw
    end type

    class(string_value), allocatable :: val  ! Polymorphic variable

    allocate(val)
    select type(val)
    type is(string_value)
        val%raw = "Hello, Fortran!"
    end select

    if (val%raw /= "Hello, Fortran!") error stop
end program
