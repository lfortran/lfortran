program random_number_05
    implicit none
    integer, parameter :: qp = 16
    real(qp) :: x, a(50), m(4, 5), s
    integer :: i, j, nbig
    integer(8) :: lo
    logical :: low_bits

    s = 0
    nbig = 0
    low_bits = .false.
    do i = 1, 200
        call random_number(x)
        if (x < 0.0_qp .or. x >= 1.0_qp) error stop "scalar out of [0,1)"
        if (x > 0.5_qp) nbig = nbig + 1
        s = s + x
        ! not representable as a double: uses the extra precision. Such a
        ! value has nonzero bits among the low 60 of its 112 significand
        ! bits, the low 8 bytes on a little-endian target. Read them as an
        ! integer: --fast may fold a round trip through real(8) away.
        lo = transfer(x, 0_8)
        if (iand(lo, 2_8**60 - 1) /= 0) low_bits = .true.
    end do
    if (nbig < 50 .or. nbig > 150) error stop "scalar not spread over [0,1)"
    if (s < 60.0_qp .or. s > 140.0_qp) error stop "scalar mean is off"
    if (.not. low_bits) error stop "scalar lacks quad precision"

    call random_number(a)
    do i = 1, size(a)
        if (a(i) < 0.0_qp .or. a(i) >= 1.0_qp) error stop "array out of [0,1)"
    end do
    if (maxval(a) < 0.5_qp) error stop "array too small"
    if (minval(a) > 0.5_qp) error stop "array too large"
    if (all(a == a(1))) error stop "array constant"

    call random_number(m)
    do j = 1, 5
        do i = 1, 4
            if (m(i, j) < 0.0_qp .or. m(i, j) >= 1.0_qp) error stop "2d array out of [0,1)"
        end do
    end do
    if (m(1, 1) == m(2, 2) .and. m(1, 2) == m(2, 1)) error stop "2d array constant"
    print *, "ok"
end program
