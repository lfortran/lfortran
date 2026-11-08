module gpu_lanes_nested_01_m
! Loops inside a `do concurrent` of few iterations that the lanes of a
! SIMD-group share out (gpu_vector_lanes.cpp), in the forms a language
! model's kernels write them: a nested `do concurrent`, of one index and of
! two, with a `local` variable, and with a `reduce` down a column, which the
! lanes combine (GpuLaneReduce); one along a row is left whole on each lane.
! Every result but the normalized rows, whose sums of squares the lanes add
! in another order, is compared exactly with the host's.
implicit none
contains
    ! The normalization of the rows of x by their root mean square.
    pure subroutine norm(x, y, z, eps)
        real, contiguous, intent(in) :: x(:, :)
        real, contiguous, intent(out) :: y(:, :), z(:)
        real, intent(in) :: eps
        integer :: m
        do concurrent (m = 1:size(x, 2))
            block
                real :: rrms, total
                integer :: d
                rrms = 1/sqrt(sum(x(:, m)**2)/size(x, 1) + eps)
                total = 0
                do concurrent (d = 1:size(x, 1)) reduce(+:total)
                    total = total + x(d, m)
                end do
                z(m) = total
                do concurrent (d = 1:size(x, 1))
                    block
                        real :: normalized
                        normalized = x(d, m)*rrms
                        y(d, m) = normalized
                    end block
                end do
            end block
        end do
    end subroutine

    pure subroutine nests(q, out, two, sums)
        real, contiguous, intent(in) :: q(:, :, :)
        real, contiguous, intent(out) :: out(:, :, :), two(:, :, :)
        real, contiguous, intent(out) :: sums(:, :)
        integer :: h
        do concurrent (h = 1:size(q, 3))
            block
                real :: t, acc
                integer :: i, j
                do concurrent (i = 1:size(q, 1), j = 1:size(q, 2)) local(t)
                    t = 2*q(i, j, h)
                    two(i, j, h) = t + 1
                end do
                do j = 1, size(q, 2)
                    block
                        real :: v
                        v = q(1, j, h)*3
                        out(1, j, h) = v
                    end block
                end do
                do i = 2, size(q, 1)
                    acc = 0
                    do j = 1, size(q, 2)
                        acc = acc + q(i, j, h)
                    end do
                    out(i, 1:size(q, 2), h) = acc
                end do
                acc = 0
                do concurrent (j = 1:size(q, 2)) reduce(+:acc)
                    acc = acc + q(1, j, h)
                end do
                sums(1, h) = acc
                sums(2, h) = 0
            end block
        end do
    end subroutine
end module

program gpu_lanes_nested_01
use gpu_lanes_nested_01_m
implicit none
integer, parameter :: n = 300, nr = 4, ni = 5, nj = 70, nh = 6
real :: x(n, nr), y(n, nr), z(nr), rrms, q(ni, nj, nh), out(ni, nj, nh)
real :: two(ni, nj, nh), sums(2, nh), acc
integer :: i, j, h
do j = 1, nr
    do i = 1, n
        x(i, j) = real(mod(5*i + j, 9)) - 4
    end do
end do
call norm(x, y, z, 1e-6)
do j = 1, nr
    if (z(j) /= sum(x(:, j))) error stop "column reduce"
    rrms = 1/sqrt(sum(x(:, j)**2)/n + 1e-6)
    do i = 1, n
        if (abs(y(i, j) - x(i, j)*rrms) > 1e-5*abs(x(i, j)*rrms)) &
            error stop "norm"
    end do
end do
do h = 1, nh
    do j = 1, nj
        do i = 1, ni
            q(i, j, h) = real(mod(i + 3*j + 7*h, 11))
        end do
    end do
end do
call nests(q, out, two, sums)
do h = 1, nh
    do j = 1, nj
        do i = 1, ni
            if (two(i, j, h) /= 2*q(i, j, h) + 1) error stop "two indices"
        end do
        if (out(1, j, h) /= 3*q(1, j, h)) error stop "block"
    end do
    do i = 2, ni
        acc = sum(q(i, :, h))
        if (any(out(i, :, h) /= acc)) error stop "reused variable"
    end do
    if (sums(1, h) /= sum(q(1, :, h))) error stop "reduce"
end do
print *, "ok"
end program
