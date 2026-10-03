program modulo_02
    integer :: a(0:1), n
    integer(8) :: i, big

    n = 2
    big = 9007199254740993_8
    i = big
    a = [10, 20]

    a(0) = a(modulo(i, n))
    if (a(0) /= 20) error stop
    if (modulo(big, 2) /= 1_8) error stop
    if (modulo(-big, 2) /= 1_8) error stop
    if (modulo(big, -2) /= -1_8) error stop
    if (modulo(17_8, 3) /= 2_8) error stop
    if (modulo(-17_8, 3) /= 1_8) error stop
end program
