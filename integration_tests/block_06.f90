program block_06
    implicit none
    integer :: a, b
    real :: c, d
    a = 5
    b = -5
    c = 0.5
    d = 7.5
    block
        integer :: x, y
        real :: z, w
        x = 10
        y = -10
        z = 2.25
        w = -2.25
        print *, x, y, z, w
        print *, a, b, c, d

        a = x
        b = y
        c = z
        d = w
    end block

    print *, a, b, c, d
end program block_06
