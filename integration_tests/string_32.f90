module string32_mod

    type :: string_type
        sequence
        character(len=:), allocatable :: raw
    end type string_type

    interface operator(//)
        module procedure :: compare_string_type_with_char
    end interface

    contains

    pure function compare_string_type_with_char(s1, s2) result(res)
        type(string_type), intent(in) :: s1
        character(len=*), intent(in) :: s2
        type(string_type) :: res
        res%raw = s1%raw//s2
    end function

end module

program string_32
    use string32_mod, only: string_type, operator(//)
    implicit none

    type(string_type) :: a
    character(len=10) :: b
    type(string_type) :: res
    a%raw = "hi"
    b = "bye"

    res = a // b
    print *, res%raw

    if (res%raw /= "hibye") error stop

end program
