program intrinsics_187
    use iso_fortran_env, only: dp => real64
    integer :: x, y, z
    integer(dp) :: a, b, c

    x = 44
    y = -501
    z = 0

    a = 5468272828_dp
    b = -3526282829_dp
    c = 83983_dp

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

    print *, popcnt(5468272828_dp)
    if(popcnt(5468272828_dp) /= 19) error stop

    print *, popcnt(b)
    if(popcnt(b) /= 48) error stop

    print *, popcnt(-3526282829_dp)
    if(popcnt(-3526282829_dp) /= 48) error stop

    print *, popcnt(c)
    if(popcnt(c) /= 7) error stop

    print *, popcnt(83983_dp)
    if(popcnt(83983_dp) /= 7) error stop

    print *, kind(popcnt(-501))
    if(kind(popcnt(-501)) /= 4) error stop

    print *, kind(popcnt(y))
    if(kind(popcnt(y)) /= 4) error stop

    print *, kind(popcnt(5468272828_dp))
    if(kind(popcnt(5468272828_dp)) /= 4) error stop

end program 
