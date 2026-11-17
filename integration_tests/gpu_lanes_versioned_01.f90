module gpu_lanes_versioned_01_m
implicit none
contains
    ! Column (v, h) of each matrix st(:, :, h): two sums over the column, a
    ! value that one lane writes, and the column updated in place. The
    ! number of matrices sets how many lanes a launch gives an iteration on
    ! an NVIDIA GPU (a block, a warp or one), and the kernel is written
    ! apart for a warp; it ends in the update, after which no lane waits.
    subroutine step(x, st, o)
        real, intent(in) :: x(:)
        real, intent(inout) :: st(:, :, :), o(:, :)
        integer :: h, v
        do concurrent (h = 1:size(st, 3), v = 1:size(st, 2))
            block
                real :: s, q, d
                integer :: j
                s = 0
                q = 0
                do j = 1, 128
                    s = s + st(j, v, h) * x(j)
                    q = q + x(j) * x(j)
                end do
                d = real(v) - s + q
                o(v, h) = d
                do j = 1, 128
                    st(j, v, h) = st(j, v, h) + x(j) * d
                end do
            end block
        end do
    end subroutine

end module

program gpu_lanes_versioned_01
! Every value is a small integer, so the sums are exact in any order.
use gpu_lanes_versioned_01_m, only: step
implicit none
integer :: sizes(3), k, nh, h, v, j
real :: x(128), d
real, allocatable :: st(:, :, :), st0(:, :, :), o(:, :)
sizes = [1, 48, 1024]
do j = 1, 128
    x(j) = real(mod(j, 3) - 1)
end do
do k = 1, size(sizes)
    nh = sizes(k)
    allocate(st(128, 128, nh), st0(128, 128, nh), o(128, nh))
    do h = 1, nh
        do v = 1, 128
            do j = 1, 128
                st(j, v, h) = real(mod(j + 2*v + 3*h, 5) - 2)
            end do
        end do
    end do
    st0 = st
    call step(x, st, o)
    do h = 1, nh
        do v = 1, 128
            d = real(v) - sum(st0(:, v, h) * x) + sum(x * x)
            if (o(v, h) /= d) error stop 1
            if (any(st(:, v, h) /= st0(:, v, h) + x * d)) error stop 2
        end do
    end do
    deallocate(st, st0, o)
end do
print *, "ok"
end program
