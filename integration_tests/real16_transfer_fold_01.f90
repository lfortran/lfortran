program real16_transfer_fold_01
    ! transfer of real(16) named constants, scalar and array, folds their
    ! binary128 bytes, also into a named constant
    implicit none
    real(16), parameter :: q(2) = [1.5_16, -2.25_16], qs = 1.5_16
    integer(8), parameter :: fq(4) = transfer(q, [0_8])
    integer(8), parameter :: fs(2) = transfer(qs, [0_8])
    integer(8), parameter :: expected(4) = &
        [0_8, 4611545280939032576_8, 0_8, -4611650834055299072_8]
    integer(8) :: g(4), h(2)
    real(16) :: r(2), rs

    if (any(fq /= expected)) error stop
    if (any(fs /= expected(1:2))) error stop
    g = transfer(q, [0_8])
    if (any(g /= expected)) error stop
    h = transfer(qs, [0_8])
    if (any(h /= expected(1:2))) error stop
    r = transfer(transfer(q, [0_8]), r)
    if (any(r /= q)) error stop
    rs = transfer(transfer(qs, [0_8]), rs)
    if (rs /= qs) error stop
    r = transfer(fq, r)
    if (any(r /= q)) error stop
    print *, fq, r
end program
