program intrinsics_193
    use iso_fortran_env, only: dp => real64
    integer :: x, y, z
    integer(dp) :: a, b, c

    x = 44
    y = -501
    z = 0

    a = 5468272828_dp
    b = -3526282829_dp
    c = 83983_dp

    print *, poppar(x)
    if(poppar(x) /= 1) error stop

    print *, poppar(44)
    if(poppar(44) /= 1) error stop

    print *, poppar(y)
    if(poppar(y) /= 0) error stop

    print *, poppar(-501)
    if(poppar(-501) /= 0) error stop

    print *, poppar(z)
    if(poppar(z) /= 0) error stop

    print *, poppar(0)
    if(poppar(0) /= 0) error stop

    print *, poppar(a)
    if(poppar(a) /= 1) error stop

    print *, poppar(5468272828_dp)
    if(poppar(5468272828_dp) /= 1) error stop

    print *, poppar(b)
    if(poppar(b) /= 0) error stop

    print *, poppar(-3526282829_dp)
    if(poppar(-3526282829_dp) /= 0) error stop

    print *, poppar(c)
    if(poppar(c) /= 1) error stop

    print *, poppar(83983_dp)
    if(poppar(83983_dp) /= 1) error stop

    print *, kind(poppar(-501))
    if(kind(poppar(-501)) /= 4) error stop

    print *, kind(poppar(y))
    if(kind(poppar(y)) /= 4) error stop

    print *, kind(poppar(5468272828_dp))
    if(kind(poppar(5468272828_dp)) /= 4) error stop

end program
