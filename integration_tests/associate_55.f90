! Issue: https://github.com/lfortran/lfortran/issues/11906
! ASSOCIATE on a component whose type is only transitively imported
! (use outer, only: outer_t — component type not imported) must resolve
! member access on the associate name.
module associate_55_inner
    implicit none
    type :: inner_t
        integer :: n = 0
    end type
end module

module associate_55_outer
    use associate_55_inner, only: inner_t
    implicit none
    type :: outer_t
        type(inner_t) :: c
    end type
end module

module associate_55_driver
    use associate_55_outer, only: outer_t
    implicit none
contains
    integer function get_n(s) result(x)
        type(outer_t), intent(in) :: s
        associate (a => s%c)
            x = a%n
        end associate
    end function
end module

program associate_55
    use associate_55_outer, only: outer_t
    use associate_55_driver, only: get_n
    implicit none
    type(outer_t) :: s
    integer :: x

    s%c%n = 42

    ! Associate in the main program (component type not imported)
    associate (a => s%c)
        if (a%n /= 42) error stop 1
        a%n = 7
    end associate
    if (s%c%n /= 7) error stop 2

    ! Associate inside a module procedure (three-module chain)
    x = get_n(s)
    if (x /= 7) error stop 3

    print *, "ok"
end program associate_55
