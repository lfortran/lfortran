program scalar_allocation_check_05
    implicit none

    type m
        integer, allocatable :: x
    end type

    type(m) :: m1

    print *, m1%x
end
