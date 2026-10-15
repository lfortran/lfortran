program data_28
    ! Large implied-DO DATA tables of procedure and BLOCK locals
    implicit none
    integer :: i
    do i = 1, 3
        call tables(i)
    end do
contains
    subroutine tables(call_number)
        integer, intent(in) :: call_number
        integer :: a(20000), b(100, 200), c(3), i, j
        real :: x(20000)
        data (a(i), i=1,20000) /10000*1, 10000*2/
        data ((b(i, j), i=1,100), j=1,200) /20000*3/
        data (c(i), x(i), i=1,3) /4, 1.5, 5, 2.5, 6, 3.5/
        if (a(1) /= call_number .or. sum(a(2:)) /= 29999) error stop "a"
        if (b(1, 1) /= 2 + call_number .or. sum(b) /= 59999 + call_number) &
            error stop "b"
        if (any(c /= [4, 5, 6])) error stop "c"
        if (any(x(1:4) /= [1.5, 2.5, 3.5, 0.0])) error stop "x"
        a(1) = a(1) + 1
        b(1, 1) = b(1, 1) + 1
        block
            integer :: d(5000), k
            data (d(k), k=1,5000) /2500*7, 2500*8/
            if (d(1) /= 6 + call_number .or. sum(d(2:)) /= 37493) &
                error stop "d"
            d(1) = d(1) + 1
        end block
    end subroutine
end program
