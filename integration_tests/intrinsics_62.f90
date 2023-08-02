program intrinsics_62
    implicit none

    type t
        integer          :: x, y
    end type t
    integer              :: x1(2), res_01(1)
    integer, allocatable :: x2(:), x3(:, :)
    real                 :: y(3, 4)
    character(3)         :: z
    type(t)              :: type_1(1)
    type(t), allocatable :: type_2(:)
    integer(8)           :: res_02(2)

    res_01 = shape(x1)
    if (res_01(1) /= 2) error stop

    allocate(x2(3))
    res_01 = shape(x2)
    if (res_01(1) /= 3) error stop

    ! allocate(x2(10, 11))
    ! res_02 = shape(x2, 8)
    ! if (res_01(1) /= 10 .and. res_02(2) /= 11) error stop

    res_02 = shape(y, 8)
    if (res_02(1) /= 3 .and. res_02(2) /= 4) error stop

    ! res_01 = shape(type_1)
    ! if (res_01(1) /= 1) error stop

    ! allocate(type_2(4))
    ! res_01 = shape(type_2)
    ! if (res_01(1) /= 4) error stop

    ! if (size(shape(z)) /= 0) error stop
end program intrinsics_62
