program main
    integer :: a, b, c(5), d
    common / block_1 / a, b, c
    a = 2
    b = 1
    d = b
    c = 4
    c(1) = 2
    b = c(3)
    print *, a, b, c, d, c(5)
end program