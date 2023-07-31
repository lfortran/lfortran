program arrays_28
    implicit none
    integer                 :: dim = 1, i, j, n = 6
    integer(4)              :: res_01(1), res_02(2)
    integer(8)              :: res_03(1)
    integer(4)              :: arr_01(6) = [-14, 3, 0, -2, 19, 1]
    integer(4)              :: arr_02(3, 3) = reshape( &
        [-14, 30, -23, 0, -2, 1, -17, 7, -13], [3, 3])
    integer(8)              :: arr_03(6) = [-14_8, 3_8, 20_8, -2_8, 19_8, 1_8]
    real   (4)              :: arr_04(6) = [-14., 3., 0., -2., 19., 25.]
    integer(4), allocatable :: arr_05(:)
    real   (8), allocatable :: arr_06(:)

    allocate(arr_05(n))
    allocate(arr_06(n))

    do i = 1, n
        arr_05(i) = arr_01(n - i + 1)
    end do

    do i = 1, n
        arr_06(i) = arr_04(n - i + 1)
    end do

    ! MinLoc
    if (minloc(arr_01, dim) /= 1) error stop
    if (minloc(arr_05, dim=1) /= 6) error stop

    ! MaxLoc
    if (maxloc(arr_01, 1) /= 5) error stop

    res_02 = maxloc(arr_02)
    if (res_02(1) /= 2 .and. res_02(2) /= 1) error stop

    res_03 = maxloc(arr_03)
    if (res_03(1) /= 3) error stop

    res_01 = maxloc(arr_04)
    if (res_01(1) /= 6) error stop

    if (maxloc(arr_05, dim=1) /= 2) error stop

    res_03 = maxloc(arr_06)
    if (res_03(1) /= 1) error stop
end program arrays_28
