program intrinsics_132
    real, parameter :: a(4) = [11.012, -21.125, 31.210, -41.0]
    real, parameter :: b(4) = [33.9, 91.2, 37.4, 19.1]
    real, parameter :: c(8) = [1.2, 3.3, 5.0, 6.8, -5.7, -8.9, 0.0, -412.124]
    real :: x = 5.8
    integer, parameter :: array_size = 6
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    integer :: test_kind
    integer(kind=4) :: res_4_arr(array_size)
    integer(kind=8) :: res_8_arr(array_size)
    integer(kind=4) :: res_4_arr_2(8)

    print *, floor(a)
    if (any(floor(a) /= [11, -22, 31, -41])) error stop

    print *, floor(b)
    if (any(floor(b) /= [33, 91, 37, 19])) error stop

    print *, floor(c)
    if (any(floor(c) /= [1, 3, 5, 6, -6, -9, 0, -413])) error stop

    res_4_arr = floor([real:: 1.2, 3.3, 5, 6.8, -5.7, -8.9])
    print *, res_4_arr
    if (any(res_4_arr /= [1, 3, 5, 6, -6, -9])) error stop

    res_8_arr = floor([real(8) :: 1.2, 3.3, 5, 101.768, -121.321, 65.4], kind=8)
    print *, res_8_arr
    if (any(res_8_arr /= [1, 3, 5, 101, -122, 65])) error stop

    res_4_arr_2 = floor([1.2, 3.3, 5.0, 6.8, -5.7, -8.9, 0.0, -412.124], kind=4)
    print *, res_4_arr_2
    if (any(res_4_arr_2 /= [1, 3, 5, 6, -6, -9, 0, -413])) error stop
    
    res_4 = floor(x)
    print *, res_4
    if (res_4 /= 5) error stop

    res_8 = floor(x, 8)
    print *, res_8
    if (res_8 /= 5) error stop

    res_4 = floor(x, 4)
    print *, res_4
    if (res_4 /= 5) error stop

    res_4 = floor(5.8)
    print *, res_4
    if (res_4 /= 5) error stop

    res_8 = floor(5.8, 8)
    print *, res_8
    if (res_8 /= 5) error stop

    res_4 = floor(0.0)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = floor(0.0, 8)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = floor(-412.124)
    print *, res_4
    if (res_4 /= -413) error stop

    res_8 = floor(-412.124, 8)
    print *, res_8
    if (res_8 /= -413) error stop

    test_kind = kind(floor(x,4))
    print *, test_kind
    if (test_kind /= 4) error stop

    test_kind = kind(floor(x,8))
    print *, test_kind
    if (test_kind /= 8) error stop

    test_kind = kind(floor(5.0,4))
    print *, test_kind
    if (test_kind /= 4) error stop

    test_kind = kind(floor(5.0,8))
    print *, test_kind
    if (test_kind /= 8) error stop

    test_kind = kind(floor(0.0))
    print *, test_kind
    if (test_kind /= 4) error stop
    
end program
