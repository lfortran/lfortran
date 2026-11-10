program gpu_lanes_block_01
! A launch of few iterations that each sum down a column of a matrix gives the
! lanes of an iteration the threads of a whole block on an NVIDIA GPU, which
! combine their sums through shared memory (gpu_vector_lanes.cpp), and a
! loop whose step is a run-time value -- the loop of one lane -- is counted
! by its number of iterations. The sums run over fewer, as many as and more
! rows than a warp, a block and the unrolling of a loop have, and a matrix-vector
! product over columns streams down a row of the matrix with a deep unrolling.
! The values are small integers, so every sum is exact.
implicit none
integer, parameter :: dp = kind(0.d0)
integer :: rows(10), cols(8), k, m, n, i, j
real(dp), allocatable :: a(:, :), s(:), x(:), y(:)
real(dp) :: t, ref

rows = [1, 2, 31, 32, 33, 255, 256, 257, 1000, 4099]
do k = 1, size(rows)
    m = rows(k)
    n = 300
    allocate(a(m, n), s(n))
    do j = 1, n
        do i = 1, m
            a(i, j) = real(mod(i*3 + j, 11) - 5, dp)
        end do
    end do
    s = 1
    do concurrent (j = 1:n)
        block
            real(dp) :: acc
            integer :: r
            acc = 0
            do r = 1, m
                acc = acc + a(r, j)
            end do
            s(j) = 0.5_dp*s(j) + acc
        end block
    end do
    do j = 1, n
        ref = 0
        do i = 1, m
            ref = ref + a(i, j)
        end do
        if (s(j) /= 0.5_dp + ref) then
            print *, "column sum", m, j, s(j), 0.5_dp + ref
            error stop 1
        end if
    end do
    deallocate(a, s)
end do

cols = [1, 15, 16, 17, 63, 64, 65, 1001]
do k = 1, size(cols)
    n = cols(k)
    m = 5000
    allocate(a(m, n), x(n), y(m))
    do j = 1, n
        x(j) = real(mod(j, 5) - 2, dp)
        do i = 1, m
            a(i, j) = real(mod(i + 2*j, 7) - 3, dp)
        end do
    end do
    y = 1
    do concurrent (i = 1:m)
        block
            real(dp) :: acc
            integer :: c
            acc = 0
            do c = 1, n
                acc = acc + a(i, c)*x(c)
            end do
            y(i) = 0.5_dp*y(i) + acc
        end block
    end do
    do i = 1, m
        t = 0
        do j = 1, n
            t = t + a(i, j)*x(j)
        end do
        if (y(i) /= 0.5_dp + t) then
            print *, "matrix-vector", n, i, y(i), 0.5_dp + t
            error stop 2
        end if
    end do
    deallocate(a, x, y)
end do
print *, "ok"
end program
