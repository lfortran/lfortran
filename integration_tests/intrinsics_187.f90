program intrinsics_187
    implicit none
    integer :: x, y, z
    integer(8) :: a, b, c
    integer, parameter :: x1 = popcnt(44)
    integer(8), parameter :: y1 = popcnt(5468272828_8)
    integer, parameter :: z1 = popcnt(-501)
    integer(8), parameter :: w1 = popcnt(-3526282829_8)
    integer, parameter :: ar1(3) = popcnt([83983, 5468272, -3526282])
    integer(8), parameter :: ar2(3) = popcnt([83983_8, 5468272828_8, -3526282829_8])

    print *, x1
    if(x1 /= 3) error stop
    print *, y1
    if(y1 /= 19) error stop
    print *, z1
    if(z1 /= 26) error stop
    print *, w1
    if(w1 /= 48) error stop
    print *, ar1
    if(any(ar1 /= [7, 10, 20])) error stop
    print *, ar2
    if(any(ar2 /= [7, 19, 48])) error stop

    x = 44
    y = -501
    z = 0

    a = 5468272828_8
    b = -3526282829_8
    c = 83983_8

    print *, popcnt(x)
    if(popcnt(x) /= 3) error stop

    print *, popcnt(44)
    if(popcnt(44) /= 3) error stop

    print *, popcnt(y)
    if(popcnt(y) /= 26) error stop

    print *, popcnt(-501)
    if(popcnt(-501) /= 26) error stop

    print *, popcnt(z)
    if(popcnt(z) /= 0) error stop
    print *, popcnt(0)
    if(popcnt(0) /= 0) error stop

    print *, popcnt(a)
    if(popcnt(a) /= 19) error stop

    print *, popcnt(5468272828_8)
    if(popcnt(5468272828_8) /= 19) error stop

    print *, popcnt(b)
    if(popcnt(b) /= 48) error stop

    print *, popcnt(-3526282829_8)
    if(popcnt(-3526282829_8) /= 48) error stop

    print *, popcnt(c)
    if(popcnt(c) /= 7) error stop

    print *, popcnt(83983_8)
    if(popcnt(83983_8) /= 7) error stop

    print *, kind(popcnt(-501))
    if(kind(popcnt(-501)) /= 4) error stop

    print *, kind(popcnt(y))
    if(kind(popcnt(y)) /= 4) error stop

    print *, kind(popcnt(5468272828_8))
    if(kind(popcnt(5468272828_8)) /= 4) error stop

end program 
