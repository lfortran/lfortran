module array_bounds_check_13_mod
    type :: base
        integer :: x
    end type base

    type, extends(base) :: derived
        integer :: y
    end type derived

    contains
        subroutine my(a, b)
            class(base), intent(in) :: a(:)
            integer, optional :: b

            print *, a(1)%x
        end subroutine
end module

program array_bounds_check_13
    use array_bounds_check_13_mod

    integer, allocatable :: i
    type(derived), allocatable :: my_array(:)

    i = 10

    call my(my_array, i)
end program
