program arrays_reshape_39
    implicit none
    integer(8), parameter :: d1_int8(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(8) :: d4_int8(3, 2, 3, 2) = reshape(d1_int8, [3, 2, 3, 2], [integer(8) :: 3])

    ! First 18 elements filled from d1_int8 (column-major order)
    if (d4_int8(1,1,1,1) /= -10) error stop
    if (d4_int8(2,1,1,1) /= 2)   error stop
    if (d4_int8(3,1,1,1) /= 3)   error stop
    if (d4_int8(1,2,1,1) /= 4)   error stop
    if (d4_int8(2,2,1,1) /= -6)  error stop
    if (d4_int8(3,2,1,1) /= 6)   error stop
    if (d4_int8(1,1,2,1) /= -7)  error stop
    if (d4_int8(2,1,2,1) /= 8)   error stop
    if (d4_int8(3,1,2,1) /= 9)   error stop
    if (d4_int8(1,2,2,1) /= 4)   error stop
    if (d4_int8(2,2,2,1) /= 1)   error stop
    if (d4_int8(3,2,2,1) /= -20) error stop
    if (d4_int8(1,1,3,1) /= 9)   error stop
    if (d4_int8(2,1,3,1) /= 10)  error stop
    if (d4_int8(3,1,3,1) /= 14)  error stop
    if (d4_int8(1,2,3,1) /= 15)  error stop
    if (d4_int8(2,2,3,1) /= 40)  error stop
    if (d4_int8(3,2,3,1) /= 30)  error stop

    ! Remaining 18 elements filled with pad value 3
    if (d4_int8(1,1,1,2) /= 3) error stop
    if (d4_int8(2,1,1,2) /= 3) error stop
    if (d4_int8(3,1,1,2) /= 3) error stop
    if (d4_int8(1,2,1,2) /= 3) error stop
    if (d4_int8(3,2,3,2) /= 3) error stop

end program arrays_reshape_39
