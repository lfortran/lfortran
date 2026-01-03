program loop_unroll_small

    implicit none
    integer :: i, first, last, array(4), x

    do i = 1, 4
        array(i) = i
    end do

    first = 1
    last = 4
    do i = first, last
        array(i) = array(i) + i
    end do

    do i = 1, 4
        x = array(i)
        call print_subrout(x)
    end do

    do i = 1, 4
        x = array(i)
        if( x /= 2 * i ) error stop
    end do

contains

    subroutine print_subrout(x)
        integer, intent(in) :: x
        print *, x
    end subroutine

end program

