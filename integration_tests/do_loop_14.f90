! A DO loop whose step is zero at run time is an error.
program do_loop_14
    implicit none
    integer :: i, step_value

    step_value = get_step()
    do i = 1, 3, step_value
    end do

contains
    integer function get_step()
        get_step = 0
    end function
end program
