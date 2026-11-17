module gpu_lanes_const_trips_01_m
implicit none
contains
    ! The sums over the columns of a 128 x 128 state per head, as the gated
    ! delta rule reads them: the lanes of an iteration share loops of a
    ! length known at compile time (128 and 100 iterations), which the CUDA
    ! code unrolls by the iterations each of 32 lanes takes.
    subroutine colsums(st, q, o)
        real, intent(in) :: st(:, :, :), q(:)
        real, intent(out) :: o(:, :)
        integer :: h, v
        do concurrent (h = 1:size(st, 3), v = 1:size(st, 2))
            block
                real :: s, m, t
                integer :: j
                s = 0
                m = -huge(m)
                do j = 1, 128
                    s = s + st(j, v, h)*q(j)
                    m = max(m, st(j, v, h))
                end do
                t = 0
                do j = 1, 100
                    t = t + st(j, v, h)
                end do
                o(v, h) = s + m + 2*t
            end block
        end do
    end subroutine
end module

program gpu_lanes_const_trips_01
! Every value is an integer, so the sums are exact in any order.
use gpu_lanes_const_trips_01_m, only: colsums
implicit none
integer, parameter :: d = 128, nh = 3
real :: st(d, d, nh), q(d), o(d, nh), s, m, t
integer :: h, v, j
do h = 1, nh
    do v = 1, d
        do j = 1, d
            st(j, v, h) = real(mod(j + 3*v + 7*h, 11) - 5)
        end do
    end do
end do
do j = 1, d
    q(j) = real(mod(j, 5) - 2)
end do
call colsums(st, q, o)
do h = 1, nh
    do v = 1, d
        s = 0
        m = -huge(m)
        do j = 1, d
            s = s + st(j, v, h)*q(j)
            m = max(m, st(j, v, h))
        end do
        t = sum(st(1:100, v, h))
        if (o(v, h) /= s + m + 2*t) error stop 1
    end do
end do
print *, "ok"
end program
