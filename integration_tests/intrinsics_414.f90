program main
    integer :: x

    x = transfer("", 1)
    print *, x
    if (x /= 0) error stop   ! x should be 0
end program