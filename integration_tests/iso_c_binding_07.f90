program iso_c_binding_07
    use iso_c_binding, only: c_int_least8_t, c_int_least16_t, &
        c_int_least32_t, c_int_least64_t, c_int_fast8_t, c_int_fast16_t, &
        c_int_fast32_t, c_int_fast64_t
    implicit none

    integer(c_int_least8_t) :: a
    integer(c_int_least16_t) :: b
    integer(c_int_least32_t) :: c
    integer(c_int_least64_t) :: d
    integer(c_int_fast8_t) :: e
    integer(c_int_fast16_t) :: f
    integer(c_int_fast32_t) :: g
    integer(c_int_fast64_t) :: h

    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    f = 6
    g = 7
    h = 8

    if (kind(a) /= c_int_least8_t) error stop
    if (kind(b) /= c_int_least16_t) error stop
    if (kind(c) /= c_int_least32_t) error stop
    if (kind(d) /= c_int_least64_t) error stop
    if (kind(e) /= c_int_fast8_t) error stop
    if (kind(f) /= c_int_fast16_t) error stop
    if (kind(g) /= c_int_fast32_t) error stop
    if (kind(h) /= c_int_fast64_t) error stop
end program iso_c_binding_07
