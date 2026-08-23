program character_34
    ! A negative character length specifier declares a zero length string
    character(-1) :: c
    character(len=-5) :: d
    character*(-3) :: e
    character :: f*(-2)
    character(-1) :: g(2)
    character :: h(2)*(-4)

    if (len(c) /= 0) error stop
    if (len(d) /= 0) error stop
    if (len(e) /= 0) error stop
    if (len(f) /= 0) error stop
    if (len(g) /= 0) error stop
    if (len(h) /= 0) error stop
    if (len(c // d) /= 0) error stop
    if (len(trim(c)) /= 0) error stop
end program character_34
