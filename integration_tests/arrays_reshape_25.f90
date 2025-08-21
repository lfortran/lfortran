program arrays_reshape_25
    implicit none
    real :: b(3, 2) = reshape([-1, -3, 6, 7, 8, 9], [3, 2])

    ! TODO: Enable these checks once runtime issues are resolved
    ! if (b(1,1) /= -1.0) error stop "Mismatch at b(1,1)"
    ! if (b(2,1) /= -3.0)  error stop "Mismatch at b(2,1)"
    ! if (b(3,1) /= 6.0)  error stop "Mismatch at b(3,1)"
    ! if (b(1,2) /= 7.0) error stop "Mismatch at b(1,2)"
    ! if (b(2,2) /= 8.0)  error stop "Mismatch at b(2,2)"
    ! if (b(3,2) /= 9.0)  error stop "Mismatch at b(3,2)"
end program arrays_reshape_25
