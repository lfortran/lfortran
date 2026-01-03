program intrinsics_297
    use iso_fortran_env, only: dp => real64
    integer :: x, y, z
    integer(dp) :: a, b, c

    integer, parameter :: x1 = poppar(44)
    integer(8), parameter :: y1 = poppar(5468272828_8)
    integer, parameter :: z1 = poppar(-501)
    integer(8), parameter :: w1 = poppar(-3526282829_8)
    integer, parameter :: ar1(3) = poppar([83983, 5468272, -3526282])
    integer(8), parameter :: ar2(3) = poppar([83983_8, 5468272828_8, -3526282829_8])
    integer :: arr1(3) = [83983, 5468272, -3526282]
    integer(8) :: arr2(3) = [83983_8, 5468272828_8, -3526282829_8]

    print *, x1
    if(x1 /= 1) error stop
    print *, y1
    if(y1 /= 1) error stop
    print *, z1
    if(z1 /= 0) error stop
    print *, w1
    if(w1 /= 0) error stop
    print *, ar1
    if(any(ar1 /= [1, 0, 0])) error stop
    print *, ar2
    if(any(ar2 /= [1, 1, 0])) error stop

    print *, poppar(arr1)
    if (any(poppar(arr1) /= [1, 0, 0])) error stop
    print *, poppar(arr2)
    if (any(poppar(arr2) /= [1, 1, 0])) error stop
    
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
end program