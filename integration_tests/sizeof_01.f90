program sizeof_01
    use iso_c_binding, only: c_int, c_double, c_size_t
    implicit none
    integer(c_int) :: x
    real(c_double) :: y
    integer(c_size_t) :: sx, sy

    sx = sizeof(x)
    if (sx /= 4) error stop "sizeof(c_int) should be 4"

    sy = sizeof(y)
    if (sy /= 8) error stop "sizeof(c_double) should be 8"
end program
