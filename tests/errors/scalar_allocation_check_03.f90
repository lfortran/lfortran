program scalar_allocation_check_03
    implicit none

    type :: base
        integer :: x
    end type

    type(base), allocatable :: var

    print *, var%x

end program
