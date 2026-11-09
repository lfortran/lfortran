! A module variable of a derived type whose component is an array of a
! derived type with default initialization, used from a C main: there is no
! Fortran main program to set anything up, so the translation unit that
! defines the variable has to set its components up itself.
module derived_types_255_mod
    implicit none
    type :: inner_t
        integer :: kind = 1
        real :: level = 0.8
        logical :: muted = .false.
    end type
    type :: outer_t
        integer :: active = 3
        type(inner_t) :: items(4)
    end type
    type(outer_t), save :: g
end module

subroutine derived_types_255_check() bind(c, name="derived_types_255_check")
    use derived_types_255_mod, only: g
    implicit none
    integer :: i
    print *, g%active, g%items(4)%kind, g%items(4)%level, g%items(4)%muted
    if (g%active /= 3) error stop
    do i = 1, 4
        if (g%items(i)%kind /= 1) error stop
        if (abs(g%items(i)%level - 0.8) > 1e-6) error stop
        if (g%items(i)%muted) error stop
    end do
end subroutine
