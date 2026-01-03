program more_kwargs_than_acceptable_to_subroutine
    implicit none

    call my_func(y=1, x=2, z=1)

    contains

    subroutine my_func(x, y)
        integer, intent(in) :: x, y
        print *, "hi"
    end subroutine

end program
