program cobounds_01
    implicit none

    integer :: x(5)[2:*]
    integer :: a, b
    integer :: lc(1), uc(1)

    a = lcobound(x, 1)
    b = ucobound(x, 1)

    lc = lcobound(x)
    uc = ucobound(x)

    print *, a
    print *, b
    print *, lc
    print *, uc

end program cobounds_01