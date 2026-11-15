program gpu_lanes_short_01
! The lanes of an iteration are the threads of a warp, or of a whole block
! where the iterations are few or the loops walk columns. A sum over a few
! hundred elements (the head dimension of an attention layer) leaves most of a
! block's threads idle and makes them meet at barriers, so such a launch keeps
! the lanes of a warp, and a longer loop keeps the block. A launch of few
! iterations keeps the block whatever the length, where a warp each would leave
! the GPU idle. The length is known only at run time here, in both ranges. The
! values are small integers, so every order of the additions gives the same
! bits, which are compared exactly.
implicit none
integer :: dk, rows, cols, i, j, k, which, size_case
integer, parameter :: lengths(4) = [96, 200, 1500, 3000]
integer, parameter :: row_counts(2) = [40, 3000]
real, allocatable :: w(:, :), x(:, :), y(:), yref(:)
real :: t
do size_case = 1, size(row_counts)
do which = 1, size(lengths)
    dk = lengths(which) + command_argument_count()*0
    rows = row_counts(size_case) + command_argument_count()*0
    cols = 300
    allocate(w(dk, rows), x(dk, cols), y(rows), yref(rows))
    do j = 1, rows
        do k = 1, dk
            w(k, j) = mod(3*k + 5*j, 7) - 3
        end do
    end do
    do j = 1, cols
        do k = 1, dk
            x(k, j) = mod(k + 2*j, 5) - 2
        end do
    end do
    do j = 1, rows
        t = 0
        do k = 1, dk
            t = t + w(k, j)*x(k, 3)
        end do
        yref(j) = t
    end do
    do concurrent (j = 1:rows)
        block
            real :: acc
            integer :: n
            acc = 0
            do n = 1, dk
                acc = acc + w(n, j)*x(n, 3)
            end do
            y(j) = acc
        end block
    end do
    if (any(y /= yref)) error stop "sum"
    deallocate(w, x, y, yref)
end do
end do
print *, "ok"
end program
