program gpu_array_reduction_01
! A REDUCE of an array whose elements every iteration names the same, as
! `s(1)` or `s(k)` for a `k` the loop does not change, is combined on the
! device (GpuReduceCombine): each element has an accumulator in every
! thread, the threads of a block combine theirs, and the block that finishes
! last combines the blocks' and applies them to the element, which keeps
! what it held before the loop. The sizes are around a warp, a block and the
! number of threads of a grid-stride loop; the values are small integers, so
! that every partial result is exact and the results equal those of the same
! loops on the host. An element the loop does not name is left as it was.
implicit none
integer, parameter :: dp = kind(0.d0)
integer :: sizes(13), q, n, i, k, j
real(dp), allocatable :: a(:)
real(4), allocatable :: x(:)
integer, allocatable :: ia(:)
integer(8) :: ls(3), rls(3)
real(dp) :: s(4), rs(4), p(2), rp(2), d1(2), d2(2)
real(4) :: mx(2), rmx(2), mn(3), rmn(3)
integer :: is(2), ris(2), imx(2), rimx(2), ip(1), rip(1)
real(dp) :: t(2, 2), rt(2, 2)

sizes = [1, 2, 31, 32, 33, 255, 256, 257, 1000, 65537, 262143, 262145, &
    1000003]
k = 4
do q = 1, size(sizes)
    n = sizes(q)
    allocate(a(n), x(n), ia(n))
    do i = 1, n
        a(i) = real(mod(i, 13) - 6, dp)
        x(i) = real(mod(i*3, 17) - 8)
        ia(i) = mod(i*7, 101) - 50
    end do

    ! Several elements of one array, one of them twice in an iteration and
    ! once in a loop of the iteration, one at a subscript the loop does not
    ! change, and one not at all.
    s = [10.0_dp, -5.0_dp, 0.5_dp, 100.0_dp]
    do concurrent (i = 1:n) reduce(+: s)
        s(1) = s(1) + a(i)
        s(3) = s(3) + a(i)*a(i)
        s(1) = s(1) + 1
        do j = 1, 3
            s(k) = s(k) + j
        end do
    end do
    rs = [10.0_dp, -5.0_dp, 0.5_dp, 100.0_dp]
    do i = 1, n
        rs(1) = rs(1) + a(i) + 1
        rs(3) = rs(3) + a(i)*a(i)
        rs(k) = rs(k) + 6
    end do
    if (any(s /= rs)) then
        print *, "real(8) +", n, s, rs
        error stop 1
    end if

    mx = [-7.0, 3.0]
    mn = [1.0, 2.0, 50.0]
    do concurrent (i = 1:n) reduce(max: mx) reduce(min: mn)
        mx(2) = max(mx(2), x(i))
        mn(3) = min(mn(3), x(i) + 9)
        mn(1) = min(mn(1), x(i))
    end do
    rmx = [-7.0, 3.0]
    rmn = [1.0, 2.0, 50.0]
    do i = 1, n
        rmx(2) = max(rmx(2), x(i))
        rmn(3) = min(rmn(3), x(i) + 9)
        rmn(1) = min(rmn(1), x(i))
    end do
    if (any(mx /= rmx) .or. any(mn /= rmn)) then
        print *, "real(4) max/min", n, mx, rmx, mn, rmn
        error stop 2
    end if

    is = [3, -4]
    imx = [-huge(1), 7]
    do concurrent (i = 1:n) reduce(+: is) reduce(max: imx)
        is(2) = is(2) + ia(i)
        is(1) = is(1) + (-ia(i))
        imx(1) = max(imx(1), ia(i))
    end do
    ris = [3, -4]
    rimx = [-huge(1), 7]
    do i = 1, n
        ris(2) = ris(2) + ia(i)
        ris(1) = ris(1) - ia(i)
        rimx(1) = max(rimx(1), ia(i))
    end do
    if (any(is /= ris) .or. any(imx /= rimx)) then
        print *, "integer +/max", n, is, ris, imx, rimx
        error stop 3
    end if

    ls = [1_8, 2_8, 3_8]
    do concurrent (i = 1:n) reduce(+: ls)
        ls(2) = ls(2) + int(ia(i), 8)*100000_8
    end do
    rls = [1_8, 2_8, 3_8]
    do i = 1, n
        rls(2) = rls(2) + int(ia(i), 8)*100000_8
    end do
    if (any(ls /= rls)) then
        print *, "integer(8) +", n, ls, rls
        error stop 4
    end if

    ! Products of powers of two and of signs, which are exact.
    p = [3.0_dp, 1.0_dp]
    ip = [-1]
    do concurrent (i = 1:n) reduce(*: p, ip)
        if (mod(i, 100000) == 0) p(1) = p(1) * 2
        if (mod(i, 7) == 0) ip(1) = ip(1) * (-1)
    end do
    rp = [3.0_dp, 1.0_dp]
    rip = [-1]
    do i = 1, n
        if (mod(i, 100000) == 0) rp(1) = rp(1) * 2
        if (mod(i, 7) == 0) rip(1) = rip(1) * (-1)
    end do
    if (any(p /= rp) .or. any(ip /= rip)) then
        print *, "*", n, p, rp, ip, rip
        error stop 5
    end if

    ! An element of an array of two dimensions.
    t = 1
    do concurrent (i = 1:n) reduce(+: t)
        t(2, 1) = t(2, 1) + a(i)
    end do
    rt = 1
    do i = 1, n
        rt(2, 1) = rt(2, 1) + a(i)
    end do
    if (any(t /= rt)) then
        print *, "2d array", n, t, rt
        error stop 6
    end if

    ! The same sum of values that are not exact, twice: the combination
    ! does not depend on how the threads were scheduled.
    do i = 1, n
        a(i) = sin(real(i, dp))
    end do
    d1 = 0
    do concurrent (i = 1:n) reduce(+: d1)
        d1(2) = d1(2) + a(i)
    end do
    d2 = 0
    do concurrent (i = 1:n) reduce(+: d2)
        d2(2) = d2(2) + a(i)
    end do
    if (d1(2) /= d2(2) .or. abs(d1(2) - sum(a)) > 1e-9_dp*n) then
        print *, "not deterministic", n, d1, d2, sum(a)
        error stop 7
    end if
    deallocate(a, x, ia)
end do

! A nest of two loops.
t = 0
do concurrent (j = 1:70, i = 1:300) reduce(+: t)
    t(1, 2) = t(1, 2) + real(i + 2*j, dp)
end do
if (t(1, 2) /= 70*(300*301/2) + 2*300*(70*71/2) .or. t(1, 1) /= 0 &
        .or. t(2, 1) /= 0 .or. t(2, 2) /= 0) then
    print *, "2d nest", t
    error stop 8
end if
print *, "ok"
end program
