program transfer_14
    implicit none
    integer, parameter :: i4 = kind(1), i8 = selected_int_kind(15)

    if (transfer(epsilon(1.0) * tiny(1.0), 1_i4) /= 1_i4) error stop 1
    if (transfer(nearest(0.0, 1.0), 1_i4) /= 1_i4) error stop 2
    if (transfer(transfer(1_i4, 1.0), 1_i4) /= 1_i4) error stop 3

    if (transfer(epsilon(1d0) * tiny(1d0), 1_i8) /= 1_i8) error stop 4
    if (transfer(nearest(0d0, 1d0), 1_i8) /= 1_i8) error stop 5
    if (transfer(transfer(1_i8, 1d0), 1_i8) /= 1_i8) error stop 6
end program transfer_14