program arrays_op_27
    implicit none
    integer :: R(4), V(4), U(4)
    R = 23
    V = 9
    U = 1
    R(2:4) = V(2:4) * 1 * U(2:4)

    print *, R
    if (R(1) /= 23) error stop
    if (R(2) /= 9) error stop
    if (R(3) /= 9) error stop
    if (R(4) /= 9) error stop
end program arrays_op_27
