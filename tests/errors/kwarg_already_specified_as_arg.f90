program kwarg_already_specified_as_arg
    implicit none

    call my_func(1, y=1, x=2)

    contains

    subroutine my_func(x, y)
        integer, intent(in) :: x, y
        print *, x**2
    end subroutine

end program
