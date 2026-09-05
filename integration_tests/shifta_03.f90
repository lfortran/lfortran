program shifta_03
    ! shifta is elemental, so either argument may be an array
    integer :: x(3) = [64, 32, 16], s(3) = [4, 2, 1]
    integer(8) :: b(2) = [1024_8, 256_8]
    integer :: m(2, 2) = reshape([64, 32, 16, 8], [2, 2])
    integer :: r(3), r22(2, 2)
    integer(8) :: r8(2)

    if (any(shifta([64, 32, 16], 3) /= [8, 4, 2])) error stop
    if (any(shifta(64, [4, 2, 1]) /= [4, 16, 32])) error stop
    if (sum(shifta(64, [4, 2, 1])) /= 52) error stop

    r = shifta(x, s)
    if (any(r /= [4, 8, 8])) error stop

    r = shifta(x, 2)
    if (any(r /= [16, 8, 4])) error stop

    ! the shift may have a different integer kind than the value
    r8 = shifta(b, [1, 2])
    if (any(r8 /= [512_8, 64_8])) error stop

    r22 = shifta(m, 1)
    if (any(r22 /= reshape([32, 16, 8, 4], [2, 2]))) error stop

    ! arithmetic shift keeps the sign
    if (any(shifta([-64, -32], 2) /= [-16, -8])) error stop
end program shifta_03
