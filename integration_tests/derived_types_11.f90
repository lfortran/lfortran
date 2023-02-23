module derived_types_11
    implicit none
    type :: y
        character(kind=1, len=1) :: backslash = '\'
    end type y

    type(y), public, parameter :: type_y = y()

    character(len=3) :: x  = "lf" // type_y%backslash
end module derived_types_11

program main
    use derived_types_11
    implicit none
    if (type_y%backslash /= '\') error stop
    if (x /= "lf\") error stop
end program
