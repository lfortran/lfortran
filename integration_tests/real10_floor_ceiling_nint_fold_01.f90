program real10_floor_ceiling_nint_fold_01
    ! floor, ceiling and nint of real(10) constants are folded in real(10)
    ! precision, for array and scalar named constants, initializers and
    ! expressions
    implicit none
    real(10), parameter :: c(4) = [2.0_10**61 + 2.5_10, -2.0_10**61 - 2.5_10, &
        1.5_10, -1.5_10]
    integer(8), parameter :: big = 2_8**61
    integer(8), parameter :: ef(4) = [big + 2, -big - 3, 1_8, -2_8]
    integer(8), parameter :: ec(4) = [big + 3, -big - 2, 2_8, -1_8]
    integer(8), parameter :: en(4) = [big + 3, -big - 3, 2_8, -2_8]

    integer(8), parameter :: pf(4) = floor(c, 8), pc(4) = ceiling(c, 8)
    integer(8), parameter :: pn(4) = nint(c, 8)
    integer(8), parameter :: sf = floor(c(1), 8), sc = ceiling(c(2), 8)
    integer(8), parameter :: sn = nint(c(1), 8)
    integer, parameter :: pf4(2) = floor(c(3:4)), pc4(2) = ceiling(c(3:4))
    integer, parameter :: pn4(2) = nint(c(3:4))
    integer(8) :: vf(4) = floor(c, 8), vc(4) = ceiling(c, 8), vn(4) = nint(c, 8)
    integer(8) :: f(4)
    integer :: f4(2)

    if (any(pf /= ef) .or. any(pc /= ec) .or. any(pn /= en)) error stop
    if (sf /= ef(1) .or. sc /= ec(2) .or. sn /= en(1)) error stop
    if (any(pf4 /= ef(3:4)) .or. any(pc4 /= ec(3:4)) .or. any(pn4 /= en(3:4))) error stop
    if (any(vf /= ef) .or. any(vc /= ec) .or. any(vn /= en)) error stop

    f = floor(c, 8)
    if (any(f /= ef)) error stop
    f = ceiling(c, 8)
    if (any(f /= ec)) error stop
    f = nint(c, 8)
    if (any(f /= en)) error stop
    f4 = floor(c(3:4))
    if (any(f4 /= ef(3:4))) error stop
    f4 = ceiling(c(3:4))
    if (any(f4 /= ec(3:4))) error stop
    f4 = nint(c(3:4))
    if (any(f4 /= en(3:4))) error stop
    print *, pf, pc, pn
end program
