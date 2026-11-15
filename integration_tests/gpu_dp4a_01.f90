function dp4a_host(a, b, c) bind(c, name="__dp4a") result(r)
    use iso_c_binding, only: c_int
    use iso_fortran_env, only: int64
    implicit none
    integer(c_int), value, intent(in) :: a, b, c
    integer(c_int) :: r
    integer :: j, x, y
    integer(int64) :: total

    total = int(c, int64)
    do j = 0, 3
        x = ibits(a, 8*j, 8)
        y = ibits(b, 8*j, 8)
        if (x > 127) x = x - 256
        if (y > 127) y = y - 256
        total = total + x*y
    end do
    total = modulo(total + 2147483648_int64, 4294967296_int64) - 2147483648_int64
    r = int(total, c_int)
end function

program gpu_dp4a_01
    use iso_c_binding, only: c_int
    implicit none
    interface
        pure function packed_dot(a, b, c) bind(c, name="__dp4a") result(r)
            import c_int
            integer(c_int), value, intent(in) :: a, b, c
            integer(c_int) :: r
        end function
    end interface
    integer(c_int) :: a(6), b(6), c(6), actual(6), expected(6), nested(6)
    integer :: i

    a = [int(z'01020304', c_int), int(z'80FF017F', c_int), &
         int(z'80808080', c_int), int(z'7F7F7F7F', c_int), &
         int(z'01010101', c_int), int(z'FFFFFFFF', c_int)]
    b = [int(z'04030201', c_int), int(z'7F0180FF', c_int), &
         int(z'80808080', c_int), int(z'7F7F7F7F', c_int), &
         int(z'01010101', c_int), int(z'01010101', c_int)]
    c = [7_c_int, -17_c_int, -1_c_int, 0_c_int, huge(0_c_int), -huge(0_c_int)]
    expected = [27_c_int, -16529_c_int, 65535_c_int, 64516_c_int, &
                -2147483645_c_int, 2147483645_c_int]
    do concurrent (i = 1:6)
        actual(i) = packed_dot(a(i), b(i), c(i))
        nested(i) = packed_dot(a(i), b(i), packed_dot(a(i), b(i), 0_c_int))
    end do
    print *, actual
    if (any(actual /= expected)) error stop 1
    do i = 1, 6
        if (nested(i) /= packed_dot(a(i), b(i), packed_dot(a(i), b(i), 0_c_int))) error stop 2
    end do
end program
