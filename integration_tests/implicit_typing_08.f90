program implicit_do_2
    integer :: n(3)
    do i = 1, size(n)
        n(i) = 42
    end do
    if (n(1) /= 42) error stop
    if (n(2) /= 42) error stop
    if (n(3) /= 42) error stop
    print "(3I3)", n
end program
