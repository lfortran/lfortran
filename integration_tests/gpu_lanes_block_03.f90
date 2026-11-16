module gpu_lanes_block_03_m
implicit none
contains
    ! A launch of few iterations gives each the lanes of a whole block, which
    ! combine several sums, maxima, minima and products after the loop: on
    ! an NVIDIA GPU consecutive combinations meet in shared memory together.
    subroutine stats(a, k, s, mx, mn, p, ks, kmax)
        real, intent(in) :: a(:, :)
        integer, intent(in) :: k(:, :)
        real, intent(out) :: s(:), mx(:), mn(:), p(:)
        integer, intent(out) :: ks(:), kmax(:)
        integer :: j
        do concurrent (j = 1:size(a, 2))
            block
                real :: t, u, v, w
                integer :: m, q, i
                t = 0
                u = -huge(u)
                v = huge(v)
                w = 1
                m = 0
                q = -huge(q)
                do i = 1, size(a, 1)
                    t = t + a(i, j)
                    u = max(u, a(i, j))
                    v = min(v, a(i, j))
                    w = w*(a(i, j)/abs(a(i, j)))
                    m = m + k(i, j)
                    q = max(q, k(i, j))
                end do
                s(j) = t
                mx(j) = u
                mn(j) = v
                p(j) = w
                ks(j) = m
                kmax(j) = q
            end block
        end do
    end subroutine
end module

program gpu_lanes_block_03
! Every value is an integer, so the sums are exact in any order.
use gpu_lanes_block_03_m, only: stats
implicit none
integer, parameter :: n = 8192, cols = 3
real :: a(n, cols), s(cols), mx(cols), mn(cols), p(cols)
integer :: k(n, cols), ks(cols), kmax(cols), i, j
do j = 1, cols
    do i = 1, n
        a(i, j) = real(mod(7*i + 3*j, 101) - 50)
        if (a(i, j) == 0) a(i, j) = 1
        k(i, j) = mod(11*i + j, 97) - 40
    end do
end do
call stats(a, k, s, mx, mn, p, ks, kmax)
do j = 1, cols
    if (s(j) /= sum(a(:, j))) error stop 1
    if (mx(j) /= maxval(a(:, j))) error stop 2
    if (mn(j) /= minval(a(:, j))) error stop 3
    if (p(j) /= product(a(:, j)/abs(a(:, j)))) error stop 4
    if (ks(j) /= sum(k(:, j))) error stop 5
    if (kmax(j) /= maxval(k(:, j))) error stop 6
end do
print *, "ok"
end program
