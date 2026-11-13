program integer_kind_16_pow
    implicit none
    integer(16) :: base, exponent, result, most_negative
    most_negative = -huge(0_16) - 1
    base = 2
    exponent = 70
    result = base ** exponent
    if (result /= 1180591620717411303424_16) error stop 1
    exponent = 127
    result = base ** exponent
    if (result /= most_negative) error stop 2
    exponent = -3
    result = base ** exponent
    if (result /= 0) error stop 3
    base = -1
    result = base ** exponent
    if (result /= -1) error stop 4
    base = 1
    result = base ** exponent
    if (result /= 1) error stop 5
    base = -2
    exponent = 3
    result = base ** exponent
    if (result /= -8) error stop 6
    ! A constant exponent above 8 used to take the int64 helper too.
    if (2_16 ** 70_16 /= 1180591620717411303424_16) error stop 7
end program
