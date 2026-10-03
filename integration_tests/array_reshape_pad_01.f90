program array_reshape_pad_01
    implicit none
    integer :: original(4) = [1, 2, 3, 4]
    integer :: reshaped(2, 3)
    integer :: i, j

    reshaped = reshape(original, shape=[2, 3], pad=[0])

    if (reshaped(1, 1) /= 1) error stop
    if (reshaped(2, 1) /= 2) error stop
    if (reshaped(1, 2) /= 3) error stop
    if (reshaped(2, 2) /= 4) error stop
    if (reshaped(1, 3) /= 0) error stop
    if (reshaped(2, 3) /= 0) error stop

    ! Test with multi-element pad that cycles
    reshaped = reshape(original, shape=[2, 3], pad=[-1, -2])

    if (reshaped(1, 3) /= -1) error stop
    if (reshaped(2, 3) /= -2) error stop

end program
