program intrinsics_262
    real, parameter :: a(4) = [11.012, -21.125, 31.210, -41.0]
    real, parameter :: b(4) = [33.9, 91.2, 37.4, 19.1]
    integer, parameter :: c(8) = floor([1.2, 3.3, 5.0, 6.8, -5.7, &
     -8.9, 0.0, -412.124])
    integer, parameter :: array_size = 6
    integer :: i
    integer(kind=4) :: res_4_arr(array_size)
    integer(kind=8) :: res_8_arr(array_size)
    integer(kind=4) :: res_4_arr_2(8)
    integer :: expected(8) = [1, 3, 5, 6, -6, -9, 0, -413]

    print *, floor(a)
    if (any(floor(a) /= [11, -22, 31, -41])) error stop

    print *, floor(b)
    if (any(floor(b) /= [33, 91, 37, 19])) error stop

    do i = 1, size(c)
        print *, c(i)
        if (c(i) /= expected(i)) error stop
    end do

    res_4_arr = floor([real:: 1.2, 3.3, 5, 6.8, -5.7, -8.9])
    print *, res_4_arr
    if (any(res_4_arr /= [1, 3, 5, 6, -6, -9])) error stop

    res_8_arr = floor([real(8) :: 1.2, 3.3, 5, 101.768, -121.321, 65.4], kind=8)
    print *, res_8_arr
    if (any(res_8_arr /= [1, 3, 5, 101, -122, 65])) error stop

    res_4_arr_2 = floor([1.2, 3.3, 5.0, 6.8, -5.7, -8.9, 0.0, -412.124], kind=4)
    print *, res_4_arr_2
    if (any(res_4_arr_2 /= [1, 3, 5, 6, -6, -9, 0, -413])) error stop
    
end program
