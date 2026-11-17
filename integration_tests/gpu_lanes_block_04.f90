module gpu_lanes_block_04_m
implicit none
contains
    ! A launch of few iterations gives each the lanes of a whole block. The
    ! sums and maxima of the columns of a, weighted by r = 1, 2, 3 in turn,
    ! meet in shared memory on each pass of the loop over r, which must not
    ! write them before every lane has read the previous pass.
    subroutine passes(a, s, m)
        real, intent(in) :: a(:, :)
        real, intent(out) :: s(:, :), m(:, :)
        integer :: j
        do concurrent (j = 1:size(a, 2))
            block
                real :: t, u
                integer :: i, r
                do r = 1, 3
                    t = 0
                    u = -huge(u)
                    do i = 1, size(a, 1)
                        t = t + a(i, j) * r
                        u = max(u, a(i, j) * r)
                    end do
                    s(r, j) = t
                    m(r, j) = u
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_block_04
! Every value is an integer, so the sums are exact in any order.
use gpu_lanes_block_04_m, only: passes
implicit none
real :: a(4096, 5), s(3, 5), m(3, 5)
integer :: i, j, r
do j = 1, size(a, 2)
    do i = 1, size(a, 1)
        a(i, j) = real(mod(7*i + 3*j, 101) - 50)
    end do
end do
call passes(a, s, m)
do j = 1, size(a, 2)
    do r = 1, 3
        if (s(r, j) /= sum(a(:, j)) * r) error stop 1
        if (m(r, j) /= maxval(a(:, j)) * r) error stop 2
    end do
end do
print *, "ok"
end program
