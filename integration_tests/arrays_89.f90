program arrays_89

    integer :: temp (0:2, 2:4)
    temp = 1
    call change(temp)
    print *, temp
    if( any(temp /= reshape([1, 1, 1, 1, 1, 99, 2, 1, 1], [3, 3])) ) error stop

contains

    subroutine change(x)
        integer, intent(inout) :: x(:, :)
        print *, lbound(x, 1), lbound(x, 2)
        if( lbound(x, 1) /= 1 ) error stop
        if( lbound(x, 2) /= 1 ) error stop
        x(1, 3) = 2
        x(3, 2) = 99
    end subroutine

end program arrays_89
