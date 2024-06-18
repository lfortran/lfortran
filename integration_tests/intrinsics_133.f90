program intrinsics_133
    real, parameter :: a(4) = [11.012, -21.125, 31.210, -41.0]
    real, parameter :: b(4) = [33.9, 91.2, 37.4, 19.1]
    integer, parameter :: c(8) = Ceiling([1.2, 3.3, 5.0, 6.8, -5.7, &
    -8.9, 0.0, -412.124])
    real :: x = 5.8
    real :: y = 6.0
    integer, parameter :: array_size = 6
    integer :: i
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    integer(kind=4) :: res_4_arr(array_size)
    integer(kind=8) :: res_8_arr(array_size)
    integer(kind=4) :: res_4_arr_2(8)
    integer :: expected(8) = [2, 4, 5, 7, -5, -8, 0, -412]

    print *, Ceiling(a)
    if (any(Ceiling(a) /= [12, -21, 32, -41])) error stop

    print *, Ceiling(b)
    if (any(Ceiling(b) /= [34, 92, 38, 20])) error stop

    do i = 1, size(c)
    print *, c(i)
    if (c(i) /= expected(i)) error stop
    end do

    res_4_arr = Ceiling([real:: 1.2, 3.3, 5, 6.8, -5.7, -8.9])
    print *, res_4_arr
    if (any(res_4_arr /= [2, 4, 5, 7, -5, -8])) error stop

    res_8_arr = Ceiling([real(8) :: 1.2, 3.3, 5, 101.768, -121.321, 65.4], kind=8)
    print *, res_8_arr
    if (any(res_8_arr /= [2, 4, 5, 102, -121, 66])) error stop

    res_4_arr_2 = Ceiling([1.2, 3.3, 5.0, 6.8, -5.7, -8.9, 0.0, -412.124], kind=4)
    print *, res_4_arr_2
    if (any(res_4_arr_2 /= [2, 4, 5, 7, -5, -8, 0, -412])) error stop
    
    res_4 = Ceiling(x)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(x, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(x, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = Ceiling(y)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(y, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(y, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = Ceiling(5.8)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(5.8, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(6.00, 8)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(6.00, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(0.0)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = Ceiling(0.0, 8)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = Ceiling(-412.124)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = Ceiling(-412.124, 8)
    print *, res_8
    if (res_8 /= -412) error stop

    res_4 = Ceiling(-412.00)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = Ceiling(-412.00, 8)
    print *, res_8
    if (res_8 /= -412) error stop

end program
