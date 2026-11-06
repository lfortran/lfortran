module gpu_lanes_reuse_01_m
! A loop variable that a later loop of the same `do concurrent` iteration
! reuses as the variable of a loop inside it: its value after the first
! loop is never read, so the lanes of a SIMD-group share the first loop
! out (gpu_vector_lanes.cpp) instead of leaving it to the first lane. A
! variable read after a loop that may run no iteration still keeps the
! loop whole. Every result is compared exactly.
implicit none
contains
    pure subroutine scores(q, out, last)
        real, contiguous, intent(in) :: q(:, :, :)
        real, contiguous, intent(out) :: out(:, :, :)
        integer, contiguous, intent(out) :: last(:, :)
        do concurrent (integer :: token = 1:size(q, 3), h = 1:size(q, 2))
            block
                integer :: d, t, n
                real :: s(size(q, 1)), acc
                do t = 1, size(q, 1)
                    s(t) = q(t, h, token)*t
                end do
                do d = 1, size(q, 1)
                    acc = 0
                    do t = 1, size(q, 1)
                        acc = acc + s(t)
                    end do
                    out(d, h, token) = acc + d
                end do
                n = h - 3
                do t = 1, size(q, 1)
                    s(t) = 2*q(t, h, token)
                end do
                do d = 1, n
                    do t = 1, d
                        acc = s(t)
                    end do
                end do
                last(h, token) = t
            end block
        end do
    end subroutine
end module

program gpu_lanes_reuse_01
use gpu_lanes_reuse_01_m
implicit none
integer, parameter :: nd = 200, nh = 24, nt = 2
real :: q(nd, nh, nt), out(nd, nh, nt), acc
integer :: last(nh, nt), d, h, t
do t = 1, nt
    do h = 1, nh
        do d = 1, nd
            q(d, h, t) = real(mod(d + 5*h + 3*t, 9))
        end do
    end do
end do
call scores(q, out, last)
do t = 1, nt
    do h = 1, nh
        acc = 0
        do d = 1, nd
            acc = acc + q(d, h, t)*d
        end do
        do d = 1, nd
            if (out(d, h, t) /= acc + d) error stop "out"
        end do
        if (h - 3 >= 1 .and. last(h, t) /= h - 2) error stop "last"
        if (h - 3 < 1 .and. last(h, t) /= nd + 1) error stop "last, no iteration"
    end do
end do
print *, "ok"
end program
