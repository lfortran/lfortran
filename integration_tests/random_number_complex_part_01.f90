program random_number_complex_part_01
    implicit none

    complex(8) :: z(4)

    z = (0.0_8, -1.0_8)
    call random_number(z%re)
    if (any(z%re < 0.0_8 .or. z%re >= 1.0_8)) error stop
    if (any(z%im /= -1.0_8)) error stop

    z = (2.0_8, 0.0_8)
    call random_number(z%im)
    if (any(z%im < 0.0_8 .or. z%im >= 1.0_8)) error stop
    if (any(z%re /= 2.0_8)) error stop
end program random_number_complex_part_01
