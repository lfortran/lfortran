program arrays_reshape_60
    implicit none
    integer :: m(2, 6), r, expected(2, 6)
    r = 0
    expected = -999
    expected(:, 1) = [31, 32]
    expected(:, 3) = [33, 34]
    expected(:, 5) = [35, 36]

    m = -999
    m(:, ::2) = reshape([31 + r, 32, 33, 34, 35, 36], [2, 3])
    if (any(m /= expected)) error stop 1

    m = -999
    m(:, 1:5:2) = reshape([31, 32, 33, 34, 35, 36] + r, [2, 3])
    if (any(m /= expected)) error stop 2

    m = -999
    m(:, ::2) = reshape([31, 32, 33, 34, 35, 36], [2, 3])
    if (any(m /= expected)) error stop 3
end program
