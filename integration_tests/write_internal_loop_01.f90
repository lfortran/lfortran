program write_internal_loop_01
    implicit none
    integer :: i, ndigits, count
    character(len=10) :: digits
    character(len=32) :: buf

    count = 0
    do i = 1, 300000
        ndigits = i
        write(digits, '(I0)') ndigits
        write(buf,    '(I0,":",I0)') i, ndigits
        if (i == 1 .and. digits /= "1         ") error stop 1
        if (i == 300000 .and. digits /= "300000    ") error stop 2
        count = count + 1
    end do
    if (count /= 300000) error stop 3
end program
