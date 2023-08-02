program intrinsics_62
    implicit none

    integer :: x(2)
    real :: y(3, 4)
    character(3) :: z
    type t
        integer :: x, y
    end type t
    type(t) :: type_1(1)
    integer :: res_01(1), res_02(2)

    res_01 = shape(x)
    if (res_01(1) /= 2) error stop

    res_02 = shape(y)
    if (res_02(1) /= 3 .and. res_02(2) /= 4) error stop

    res_01 = shape(type_1)
    if (res_01(1) /= 1) error stop

    if (size(shape(z)) /= 0) error stop
end program intrinsics_62
