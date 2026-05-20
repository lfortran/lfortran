! Test: COMMON variable used as adjustable array dimension bound
program common_35
    implicit none
    integer :: m, result
    common m
    m = 5
    call fill_array(result)
    if (result /= 15) error stop
    m = 3
    call fill_array(result)
    if (result /= 6) error stop
    print *, "PASS"
end program

subroutine fill_array(total)
    implicit none
    integer :: m
    common m
    integer :: a(m)
    integer :: i, total
    do i = 1, m
        a(i) = i
    end do
    total = 0
    do i = 1, m
        total = total + a(i)
    end do
end subroutine
