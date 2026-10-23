! The result length of `char_string` calls the generic `len` of the module.
! The program imports only `char`, so its own `len(t)` is the intrinsic.
module modules_88_m
    implicit none
    private
    public :: string_type, len, char
    type :: string_type
        character(:), allocatable :: raw
    end type
    interface len
        module procedure len_string
    end interface
    interface char
        module procedure char_string
    end interface
contains
    elemental function len_string(string) result(length)
        type(string_type), intent(in) :: string
        integer :: length
        if (allocated(string%raw)) then
            length = len(string%raw)
        else
            length = 0
        end if
    end function

    pure function char_string(string) result(res)
        type(string_type), intent(in) :: string
        character(len=len(string)) :: res
        res = ''
        if (allocated(string%raw)) res = string%raw
    end function
end module

program modules_88
    use modules_88_m, only: string_type, char
    implicit none
    type(string_type) :: s
    character(:), allocatable :: t
    s%raw = "hello"
    t = char(s)
    print *, len(t), t
    if (len(t) /= 5) error stop 1
    if (t /= "hello") error stop 2
end program
