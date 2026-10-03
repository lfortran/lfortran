! A generic interface that shares the name of a derived type falls back to
! the structure constructor when no specific procedure matches. Fortran
! names are case insensitive, so the fallback must work for any spelling of
! the type name, both inside the defining module (generic procedure) and
! through use association (external symbol).
module structure_constructor_args_15_mod
    implicit none
    type :: date_t
        integer :: day = 1
        integer :: year = 2008
    end type date_t
    interface date_t
        module procedure make_from_year
    end interface date_t
contains
    type(date_t) function make_from_year(y) result(r)
        integer, intent(in) :: y
        r%day = 99
        r%year = y
    end function make_from_year

    subroutine check_in_module()
        type(date_t) :: b
        b = DATE_T(7, 2002)
        if (b%day /= 7) error stop 1
        if (b%year /= 2002) error stop 2
        b = Date_T(8, 2003)
        if (b%day /= 8) error stop 3
        if (b%year /= 2003) error stop 4
        b = DATE_T(2004)
        if (b%day /= 99) error stop 5
        if (b%year /= 2004) error stop 6
    end subroutine check_in_module
end module structure_constructor_args_15_mod

program structure_constructor_args_15
    use structure_constructor_args_15_mod
    implicit none
    type(date_t) :: b
    call check_in_module()
    b = DATE_T(7, 2002)
    if (b%day /= 7) error stop 11
    if (b%year /= 2002) error stop 12
    b = Date_T(8, 2003)
    if (b%day /= 8) error stop 13
    if (b%year /= 2003) error stop 14
    b = DATE_T(2005)
    if (b%day /= 99) error stop 15
    if (b%year /= 2005) error stop 16
    print *, b%day, b%year
end program structure_constructor_args_15
