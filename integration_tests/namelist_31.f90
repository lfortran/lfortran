program namelist_31
    implicit none

    integer :: a, b, x, y

    namelist /left/ x /right/ y
    namelist /first/ a, /second/ b

    a = 10
    b = 20
    x = 1
    y = 2

    if (a /= 10) error stop
    if (b /= 20) error stop
    if (x /= 1) error stop
    if (y /= 2) error stop
    print *,"test passed"
end program namelist_31
