program kwarg_not_found
    implicit none

    call my_func(y=1, z=2)

    contains

    subroutine my_func(x, y)
        integer, intent(in) :: x, y
        print *, x**2
    end subroutine

end program
