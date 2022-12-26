module derived_types_10
    implicit none
    type :: y
        character(kind=1, len=1) :: backslash = '\'
    end type y

    type(y), public, parameter :: type_y = y()

    character(len=3) :: x  = '  ' // type_y%backslash
end module derived_types_10
