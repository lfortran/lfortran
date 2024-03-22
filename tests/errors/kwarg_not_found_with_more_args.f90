program kwarg_not_found_with_more_args 
    implicit none

    call my_func(y=1, x=2, z=1)

    contains

    subroutine my_func(x, y)
        integer, intent(in) :: x, y
        print *, "hi"
    end subroutine

end program
