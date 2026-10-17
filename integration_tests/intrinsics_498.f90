program intrinsics_498
    ! Compile-time folding of integer(8) intrinsics must keep all 64 bits.
    implicit none
    integer(8), parameter :: a(2) = [3000000000_8, 2_8]
    integer(8), parameter :: d = dot_product(a, a)
    integer(8), parameter :: m = merge_bits(-1_8, 0_8, 4294967296_8)
    integer(8), parameter :: mx = max(9007199254740993_8, 1_8)
    integer(8), parameter :: mn = min(-9007199254740993_8, 1_8)
    integer(8), parameter :: s = ibset(0_8, 40)
    logical, parameter :: t = btest(s, 40)
    logical, parameter :: f = btest(s, 8)

    print *, d, m, mx, mn, s, t, f
    if (d /= 9000000000000000004_8) error stop
    if (m /= 4294967296_8) error stop
    if (mx /= 9007199254740993_8) error stop
    if (mn /= -9007199254740993_8) error stop
    if (s /= 1099511627776_8) error stop
    if (.not. t) error stop
    if (f) error stop
end program
