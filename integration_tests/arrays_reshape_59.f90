program arrays_reshape_59
    ! integer(8) rank-2 array initialized with reshape of a default-kind
    ! integer array constructor (#5284)
    integer(8) :: x(2,2) = reshape([-1, 2, -3, 4], [2, 2])
    integer(8) :: y(2,2)
    print *, x
    if (x(1,1) /= -1_8 .or. x(2,1) /= 2_8) error stop
    if (x(1,2) /= -3_8 .or. x(2,2) /= 4_8) error stop
    y = abs(x)
    print *, y
    if (any(y(:,1) /= [1_8, 2_8])) error stop
    if (any(y(:,2) /= [3_8, 4_8])) error stop
end program
