program gpu_metal_621
    ! integer(1) and integer(2) literals, sums, products, negations and
    ! remainders as arguments of min and max, also inside min/max reductions,
    ! in loops offloaded to the GPU.
    use iso_fortran_env, only: int8, int16
    implicit none
    integer(int16) :: h(4), rh(4), ch, mn, mx
    integer(int8) :: b(4), rb(4), cb, bmn
    integer :: i

    h = [-16_int16, 1_int16, -1_int16, 64_int16]
    b = [-16_int8, 1_int8, -1_int8, 64_int8]
    ch = 5_int16
    cb = 5_int8

    do concurrent (i = 1:4)
        rh(i) = min(h(i), 5_int16)
    end do
    if (any(rh /= [-16, 1, -1, 5])) error stop 1

    do concurrent (i = 1:4)
        rb(i) = max(b(i), 0_int8)
    end do
    if (any(rb /= [0, 1, 0, 64])) error stop 2

    do concurrent (i = 1:4)
        rb(i) = min(b(i), 5_int8)
    end do
    if (any(rb /= [-16, 1, -1, 5])) error stop 3

    do concurrent (i = 1:4)
        rh(i) = min(h(i), ch * h(i))
    end do
    if (any(rh /= [-80, 1, -5, 64])) error stop 4

    do concurrent (i = 1:4)
        rb(i) = min(b(i), b(i) + cb)
    end do
    if (any(rb /= [-16, 1, -1, 64])) error stop 5

    do concurrent (i = 1:4)
        rh(i) = max(h(i), -h(i))
    end do
    if (any(rh /= [16, 1, 1, 64])) error stop 6

    do concurrent (i = 1:4)
        rb(i) = max(b(i), -b(i))
    end do
    if (any(rb /= [16, 1, 1, 64])) error stop 7

    do concurrent (i = 1:4)
        rh(i) = min(mod(h(i), 7_int16), 0_int16)
    end do
    if (any(rh /= [-2, 0, -1, 0])) error stop 8

    mn = huge(mn)
    do concurrent (i = 1:4) reduce(min: mn)
        mn = min(mn, h(i) + 1_int16)
    end do
    if (mn /= -15_int16) error stop 9

    mx = -huge(mx) - 1_int16
    do concurrent (i = 1:4) reduce(max: mx)
        mx = max(mx, 2_int16 * h(i) - ch)
    end do
    if (mx /= 123_int16) error stop 10

    bmn = huge(bmn)
    do concurrent (i = 1:4) reduce(min: bmn)
        bmn = min(bmn, -b(i))
    end do
    if (bmn /= -64_int8) error stop 11

    print *, rh, rb, mn, mx, bmn
end program
