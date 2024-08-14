program intrinsics_153
    implicit none
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: a1 = dim(11, 31)
    integer(8), parameter :: a2 = dim(11_8, 31_8)
    real, parameter :: a3 = dim(11.0, 31.0)
    real(dp), parameter :: a4 = dim(11.0_dp, 31.0_dp)
    integer, parameter :: ar1(5) = dim([11, 31, 41, 51, 61], [1, 2, 3, 4, 5])
    integer(8), parameter :: ar2(5) = dim([11_8, 31_8, 41_8, 51_8, 61_8], [1_8, 2_8, 3_8, 4_8, 5_8])
    real, parameter :: ar3(5) = dim([11.0, 31.0, 41.0, 51.0, 61.0], [1.0, 2.0, 3.0, 4.0, 5.0])
    real(dp), parameter :: ar4(5) = dim([11.0_dp, 31.0_dp, 41.0_dp, 51.0_dp, 61.0_dp], [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp])

    real :: i,j
    real(dp) :: x,y
    integer :: a, b
    integer(8) :: c, d
    real(dp) :: res_dp
    integer :: arr1(4)
    integer(8) :: arr2(4)
    real(4) :: arr3(4)
    real(8) :: arr4(4)

    i = 30.0
    j = 20.0
    x = 10.0_dp
    y = 15.0_dp
    a = 30
    b = 20
    c = 10
    d = 15

    print *, a1
    if (a1 /= 0) error stop
    print *, a2
    if (a2 /= 0) error stop
    print *, a3
    if (a3 /= 0.0) error stop
    print *, a4
    if (abs(a4 - (0.0_dp)) > 1e-14_dp) error stop
    print *, ar1
    if (any(ar1 /= [10, 29, 38, 47, 56])) error stop
    print *, ar2
    if (any(ar2 /= [10, 29, 38, 47, 56])) error stop
    print *, ar3
    if (any(abs(ar3 - [10.0, 29.0, 38.0, 47.0, 56.0]) > 1e-7)) error stop
    print *, ar4
    if (any(abs(ar4 - [10.0_dp, 29.0_dp, 38.0_dp, 47.0_dp, 56.0_dp]) > 1e-14_dp)) error stop
    
    res_dp = dim(i, j)
    print *, res_dp
    if (abs(res_dp - (10.0_dp)) > 1e-14_dp) error stop

    res_dp = dim(x, y)
    print *, res_dp
    if (abs(res_dp - (0.0_dp)) > 1e-14_dp) error stop

    print *, dim(a, b)
    if (dim(a, b) /= 10) error stop

    print *, dim(c, d)
    if (dim(c, d) /= 0) error stop

    res_dp = dim(30.0, 20.0)
    print *, res_dp
    if (abs(res_dp - (10.0_dp)) > 1e-14_dp) error stop

    print *, dim(10.0, 15.0)
    if (abs(dim(10.0, 15.0) - (0.0)) > 1e-7) error stop

    print *, dim(30, 20)
    if (dim(30, 20) /= 10) error stop

    print *, dim(10, 15)
    if (dim(10, 15) /= 0) error stop

    print *, kind(dim(30, 20))
    if (kind(dim(30, 20)) /= 4) error stop

    print *, kind(dim(10_8, 15_8))
    if (kind(dim(10_8, 15_8)) /= 8) error stop

    print *, kind(dim(i, j))
    if (kind(dim(i, j)) /= 4) error stop

    print *, kind(dim(a, b))
    if (kind(dim(a, b)) /= 4) error stop

    print *, kind(dim(c, d))
    if (kind(dim(c, d)) /= 8) error stop

    print *, kind(dim(x, y))
    if (kind(dim(x, y)) /= 8) error stop

    arr1 = [33, 12, 31, 4]
    arr2 = [13_8, 131_8, 3_8, 4_8]
    arr3 = [41.0, 11.0, 30.0, 3.0]
    arr4 = [1.0_dp, 3.0_dp, 3.0_dp, 2.0_dp]
    
    print *, dim(arr1, arr2)
    if (any(dim(arr1, arr2) /= [20, 0, 28, 0])) error stop
    print *, dim(arr3, arr4)
    if (any(abs(dim(arr3, arr4) - [40.0, 8.0, 27.0, 1.0]) > 1e-7)) error stop

end program
