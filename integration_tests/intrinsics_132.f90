program intrinsics_132
    real :: x = 5.8
    integer, parameter :: array_size = 3
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    integer :: test_kind
    integer(kind=4) :: res_4_arr(array_size)
    integer(kind=8) :: res_8_arr(array_size)
    
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

    ! Compile time broadcasting
    res_4_arr = floor([real:: 1.2, 3.3, 5])
    print *, res_4_arr(1)
    if (res_4_arr(1) /= 1) error stop
    print *, res_4_arr(2)
    if (res_4_arr(2) /= 3) error stop
    print *, res_4_arr(3)
    if (res_4_arr(3) /= 5) error stop

    res_8_arr = floor([real(8) :: 1.2, 3.3, 5], kind=8)
    print *, res_8_arr(1)
    if (res_8_arr(1) /= 1) error stop
    if (kind(res_8_arr(1)) /= 8) error stop
    print *, res_8_arr(2)
    if (res_8_arr(2) /= 3) error stop
    if (kind(res_8_arr(2)) /= 8) error stop
    print *, res_8_arr(3)
    if (res_8_arr(3) /= 5) error stop
    if (kind(res_8_arr(3)) /= 8) error stop
    
end program
