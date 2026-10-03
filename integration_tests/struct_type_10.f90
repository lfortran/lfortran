! Test that an elemental function returning a derived type with an allocatable
! component can be called on a dummy array argument and the result passed
! directly to another function (no SIGSEGV).
module struct_type_10_mod
    implicit none
    public :: string_t

    type :: string_t
        character(len=:), allocatable :: s
    end type

    interface string_t
        module procedure from_chars
    end interface

contains

    elemental function from_chars(c) result(r)
        character(len=*), intent(in) :: c
        type(string_t) :: r
        r%s = c
    end function

    function first(arr) result(r)
        type(string_t), intent(in) :: arr(:)
        type(string_t) :: r
        r = arr(1)
    end function

    function csv_first(strings) result(r)
        character(len=*), intent(in) :: strings(:)
        type(string_t) :: r
        ! Bug: SIGSEGV when elemental constructor result is passed directly.
        r = first(string_t(strings))
    end function

end module struct_type_10_mod

program struct_type_10
    use struct_type_10_mod
    implicit none
    type(string_t) :: r

    r = csv_first(["foo", "bar", "baz"])
    if (r%s /= "foo") error stop
    print *, r%s
end program struct_type_10
