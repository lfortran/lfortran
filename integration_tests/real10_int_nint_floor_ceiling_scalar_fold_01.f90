program real10_int_nint_floor_ceiling_scalar_fold_01
    ! int, nint, floor and ceiling of real(10) scalar constants are folded in
    ! real(10) precision, not through double, for named constants,
    ! initializers and expressions
    implicit none
    ! 2 - 2**-62 and 2**61 + 2.5 are not representable in double
    real(10), parameter :: c1 = 2.0_10**61 + 2.5_10, c2 = -2.0_10**61 - 2.5_10
    real(10), parameter :: c3 = 2.0_10 - 2.0_10**(-62)
    integer(8), parameter :: big = 2_8**61

    integer(8), parameter :: si8 = int(c1, 8), sf = floor(c1, 8)
    integer(8), parameter :: sc = ceiling(c2, 8), sn = nint(c1, 8)
    integer(8), parameter :: sn2 = nint(c2, 8)
    integer, parameter :: si4 = int(c3), sf4 = floor(-1.5_10)
    integer, parameter :: sc4 = ceiling(-1.5_10), sn4 = nint(c3)
    integer(2), parameter :: si2 = int(-7.75_10, 2), sn2k = nint(1.5_10, 2)
    integer(1), parameter :: sf1 = floor(-128.5_10 + 1, 1)
    integer(8) :: vi8 = int(c2, 8), vf = floor(c2, 8), vc = ceiling(c1, 8)
    integer(8) :: vn = nint(c2, 8)
    integer(8) :: f
    integer :: f4

    if (si8 /= big + 2 .or. sf /= big + 2) error stop "si8 sf"
    if (sc /= -big - 2 .or. sn /= big + 3 .or. sn2 /= -big - 3) error stop "sc sn"
    if (si4 /= 1 .or. sf4 /= -2 .or. sc4 /= -1 .or. sn4 /= 2) error stop "kind 4"
    if (si2 /= -7 .or. sn2k /= 2 .or. sf1 /= -128) error stop "kind 1, 2"
    if (vi8 /= -big - 2 .or. vf /= -big - 3 .or. vc /= big + 3) error stop "v"
    if (vn /= -big - 3) error stop "vn"

    f = int(c1, 8)
    if (f /= big + 2) error stop
    f = floor(c1, 8)
    if (f /= big + 2) error stop
    f = ceiling(c1, 8)
    if (f /= big + 3) error stop
    f = nint(c1, 8)
    if (f /= big + 3) error stop
    f4 = int(c3)
    if (f4 /= 1) error stop
    f4 = nint(-2.5_10)
    if (f4 /= -3) error stop
    print *, si8, sf, sc, sn
end program
