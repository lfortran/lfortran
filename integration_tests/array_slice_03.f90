program array_slice_03
    implicit none
    integer :: x(4)
    integer :: y(2)
    x = [1, 2, 3, 4]
    call sub(x, y)

    print *, "y: ", y(1), y(2)
    if( y(1) /= 2 ) error stop
    if( y(2) /= 3 ) error stop

contains

    subroutine sub(x,y)
        integer, intent(in) :: x(:)
        integer, intent(out) :: y(:)
        y = x(2:3)
        if( y(1) /= 2 ) error stop
        if( y(2) /= 3 ) error stop
    end subroutine sub

end program array_slice_03
