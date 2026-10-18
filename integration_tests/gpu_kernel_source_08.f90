module gpu_kernel_source_08_m
implicit none
contains
    pure subroutine row_sum(a, r)
        real, intent(in) :: a(:)
        real, intent(out) :: r
        r = sum(a)
    end subroutine
end module

program gpu_kernel_source_08
! Each loop defines `t` whole before it reads it, so the GPU offload gives
! every thread its own `t`. Late storage placement measures the actual
! gathered rows (loops 1 and 2) and alias temporaries (loops 3 and 4).
! In loops 1 and 4 the larger array spills to a private workspace slice;
! `t` stays on the stack. Neither array becomes shared.
use gpu_kernel_source_08_m, only: row_sum
implicit none
integer, parameter :: n = 4, m = 10000
real :: xr(n, 120000), xs(n, 64), w1(60000, n), w2(120000, n)
real :: t(m), y(n), r
integer :: j, k

xr = 1
xs = 2
do j = 1, n
    do k = 1, 60000
        w1(k, j) = k
    end do
    do k = 1, 120000
        w2(k, j) = k
    end do
end do

! The 480,000-byte gathered row spills; `t` fits.
do concurrent (j = 1:n)
    do k = 1, m
        t(k) = j + k
    end do
    call row_sum(xr(j, :), r)
    y(j) = r + t(m)
end do
do j = 1, n
    if (abs(y(j) - (120000 + j + m)) > 1) error stop 1
end do

! 256 bytes gathered: `t` fits.
do concurrent (j = 1:n)
    do k = 1, m
        t(k) = 2 * j + k
    end do
    call row_sum(xs(j, :), r)
    y(j) = r + t(m)
end do
do j = 1, n
    if (abs(y(j) - (128 + 2 * j + m)) > 1e-3) error stop 2
end do

! A 240,000-byte temporary for the reversal: `t` fits.
do concurrent (j = 1:n)
    do k = 1, m
        t(k) = 3 * j + k
    end do
    w1(:, j) = w1(60000:1:-1, j)
    y(j) = w1(1, j) + t(m)
end do
do j = 1, n
    if (abs(y(j) - (60000 + 3 * j + m)) > 1e-3) error stop 3
    if (abs(w1(60000, j) - 1) > 1e-3) error stop 4
end do

! The 480,000-byte reversal temporary spills; `t` fits.
do concurrent (j = 1:n)
    do k = 1, m
        t(k) = 4 * j + k
    end do
    w2(:, j) = w2(120000:1:-1, j)
    y(j) = w2(1, j) + t(m)
end do
do j = 1, n
    if (abs(y(j) - (120000 + 4 * j + m)) > 1e-3) error stop 5
    if (abs(w2(120000, j) - 1) > 1e-3) error stop 6
end do

print *, "ok"
end program
