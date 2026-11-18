module gpu_array_reduction_05_m
use, intrinsic :: iso_fortran_env, only: int64, real64
implicit none
contains
    ! x = W v, 4 rows per iteration, on lanes that share the columns; then
    ! the sum of the squares of x, in fixed point (scale 2**24), added to
    ! ss(k), which a launch adds to with atomic updates.
    subroutine produce(w, v, x, ss, k)
        real, intent(in) :: w(:, :), v(:)
        real, intent(inout) :: x(:)
        integer(int64), intent(inout) :: ss(:)
        integer, intent(in) :: k
        integer :: i
        do concurrent (i = 1:size(w, 2)/4) reduce(+: ss)
            block
                real :: a1, a2, a3, a4
                integer :: c, n
                n = 4*i - 3
                a1 = 0; a2 = 0; a3 = 0; a4 = 0
                do c = 1, size(w, 1)
                    a1 = a1 + w(c, n)*v(c)
                    a2 = a2 + w(c, n+1)*v(c)
                    a3 = a3 + w(c, n+2)*v(c)
                    a4 = a4 + w(c, n+3)*v(c)
                end do
                x(n) = a1
                x(n+1) = a2
                x(n+2) = a3
                x(n+3) = a4
                ss(k) = ss(k) + nint(((x(n)*x(n) + x(n+1)*x(n+1)) &
                    + (x(n+2)*x(n+2) + x(n+3)*x(n+3))) * 2.0**24, int64)
            end block
        end do
    end subroutine

    ! y = (W x) / sqrt(sum of the squares of x), the sum read from ss(k);
    ! the other slot is cleared for the next producer by one iteration.
    subroutine consume(w, x, ss, k, y)
        real, intent(in) :: w(:, :), x(:)
        integer(int64), intent(inout) :: ss(:)
        integer, intent(in) :: k
        real, intent(inout) :: y(:)
        integer :: i
        do concurrent (i = 1:size(w, 2))
            block
                real :: a, r
                integer :: c
                a = 0
                do c = 1, size(w, 1)
                    a = a + w(c, i)*x(c)
                end do
                r = real(real(ss(k), real64) / 2.0_real64**24)
                y(i) = a * r
                if (i == 1) ss(3 - k) = 0
            end block
        end do
    end subroutine
end module

program gpu_array_reduction_05
! A REDUCE of elements of an array whose operation gives the same result in
! any order -- a sum of integers, a maximum or a minimum of integers or of
! reals -- is combined into the elements with atomic updates, starting from
! what they hold (asr_to_gpu_c.h, GpuReduceCombine). The sums of squares of
! a transformer's residual are added in fixed point to one of two slots of
! an integer(8) array in turn: the kernel that produces them adds to slot
! k, on lanes that share a loop, and the kernel that reads it, on lanes too,
! clears the other slot in one iteration for the next producer, so the host
! never touches the array between the launches. The values are small
! integers, so every result is exact and equals that of the same loops on
! the host.
use, intrinsic :: iso_fortran_env, only: int64
use gpu_array_reduction_05_m, only: produce, consume
implicit none
integer :: sizes(7), q, n, i, k, step, c
integer, allocatable :: ia(:)
integer(int64), allocatable :: la(:)
real, allocatable :: x(:)
real(8), allocatable :: d(:)
integer :: is(3), ris(3), imx(2), rimx(2), imn(2), rimn(2)
integer(int64) :: ls(2), rls(2), lmx(1), rlmx(1)
real :: fmx(2), rfmx(2), fmn(2), rfmn(2)
real(8) :: dmx(1), rdmx(1)
integer, parameter :: nc = 512, nr = 256
real :: w1(nc, nr), w2(nr, nc), v(nc), xr(nr), y(nc), ry(nc)
integer(int64) :: ss(1024), rs

sizes = [1, 31, 33, 1000, 65537, 262145, 1000003]
do q = 1, size(sizes)
    n = sizes(q)
    allocate(ia(n), la(n), x(n), d(n))
    do i = 1, n
        ia(i) = mod(i*7, 101) - 50
        la(i) = int(ia(i), int64) * 3000000000_int64
        x(i) = real(mod(i*3, 17) - 8)
        d(i) = real(mod(i, 13) - 6, 8)
    end do

    is = [3, -4, 9]
    imx = [-huge(1), 7]
    imn = [100, 2]
    do concurrent (i = 1:n) reduce(+: is) reduce(max: imx) reduce(min: imn)
        is(2) = is(2) + ia(i)
        is(1) = is(1) + 2*ia(i)
        imx(1) = max(imx(1), ia(i))
        imn(1) = min(imn(1), ia(i))
    end do
    ris = [3, -4, 9]
    rimx = [-huge(1), 7]
    rimn = [100, 2]
    do i = 1, n
        ris(2) = ris(2) + ia(i)
        ris(1) = ris(1) + 2*ia(i)
        rimx(1) = max(rimx(1), ia(i))
        rimn(1) = min(rimn(1), ia(i))
    end do
    if (any(is /= ris) .or. any(imx /= rimx) .or. any(imn /= rimn)) then
        print *, "integer(4)", n, is, ris, imx, rimx, imn, rimn
        error stop 1
    end if

    ls = [5_int64, -7_int64]
    lmx = -huge(1_int64)
    do concurrent (i = 1:n) reduce(+: ls) reduce(max: lmx)
        ls(1) = ls(1) + la(i)
        lmx(1) = max(lmx(1), la(i))
    end do
    rls = [5_int64, -7_int64]
    rlmx = -huge(1_int64)
    do i = 1, n
        rls(1) = rls(1) + la(i)
        rlmx(1) = max(rlmx(1), la(i))
    end do
    if (any(ls /= rls) .or. any(lmx /= rlmx)) then
        print *, "integer(8)", n, ls, rls, lmx, rlmx
        error stop 2
    end if

    fmx = [-50.0, 1.5]
    fmn = [3.0, 50.0]
    dmx = -1000
    do concurrent (i = 1:n) reduce(max: fmx, dmx) reduce(min: fmn)
        fmx(1) = max(fmx(1), x(i))
        fmn(2) = min(fmn(2), x(i) - 1)
        dmx(1) = max(dmx(1), d(i) * 0.5d0)
    end do
    rfmx = [-50.0, 1.5]
    rfmn = [3.0, 50.0]
    rdmx = -1000
    do i = 1, n
        rfmx(1) = max(rfmx(1), x(i))
        rfmn(2) = min(rfmn(2), x(i) - 1)
        rdmx(1) = max(rdmx(1), d(i) * 0.5d0)
    end do
    if (any(fmx /= rfmx) .or. any(fmn /= rfmn) .or. any(dmx /= rdmx)) then
        print *, "real max/min", n, fmx, rfmx, fmn, rfmn, dmx, rdmx
        error stop 3
    end if
    deallocate(ia, la, x, d)
end do

! The producers and consumers of a residual's sum of squares, three steps.
do i = 1, nr
    do c = 1, nc
        w1(c, i) = 0
        if (mod(c + 3*i, 64) == 0) w1(c, i) = real(mod(c + i, 3) - 1)
    end do
end do
do i = 1, nc
    do c = 1, nr
        w2(c, i) = real(mod(c + i, 5) - 2)
    end do
end do
do c = 1, nc
    v(c) = real(mod(c, 7) - 3)
end do
ss = 0
k = 1
do step = 1, 3
    k = 3 - k
    call produce(w1, v, xr, ss, k)
    call consume(w2, xr, ss, k, y)
    v = v + 1
end do
! The last step, on the host.
v = v - 1
rs = 0
do i = 1, nr
    xr(i) = sum(w1(:, i)*v)
end do
rs = sum(nint(xr*xr * 2.0**24, int64))
do i = 1, nc
    ry(i) = sum(w2(:, i)*xr) * real(real(rs, 8) / 2.0d0**24)
end do
if (ss(k) /= rs .or. ss(3 - k) /= 0 .or. any(y /= ry)) then
    print *, "residual", ss(1:2), rs
    error stop 4
end if
print *, "ok"
end program
