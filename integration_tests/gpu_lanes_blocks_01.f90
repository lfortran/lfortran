program gpu_lanes_blocks_01
! Blocks in `do concurrent` loops of few iterations (gpu_vector_lanes.cpp).
! A loop whose body is a block of scalar statements and a store has nothing
! for the lanes of a SIMD-group to share out, so it runs one thread per
! iteration, not 32 lanes that repeat the work while one of them stores.
! A block that holds no loop to share out next to a loop that has one runs
! on the first lane, and hands the others the scalars it assigns that live
! on after it (`u`), but not its own locals, which end with the block.
implicit none
integer, parameter :: n = 48, m = 300
real :: a(n), g(n), y(n), z(m, n), w(m, n), u
integer :: i, j
do i = 1, n
    a(i) = real(mod(7*i, 11)) - 5
end do
do j = 1, n
    do i = 1, m
        w(i, j) = real(mod(i + 3*j, 17))
    end do
end do
do concurrent (i = 1:n)
    block
        real :: e, s
        e = exp(-abs(a(i)))
        s = max(a(i), 0.0) + log(1.0 + e)
        g(i) = exp(-s)
    end block
end do
do concurrent (i = 1:n) local(u)
    block
        real :: t
        t = 2*a(i)
        u = t + 1
        y(i) = t*t
    end block
    z(:, i) = w(:, i)*u
end do
do i = 1, n
    if (abs(g(i) - exp(-(max(a(i), 0.0) + log(1.0 + exp(-abs(a(i))))))) &
        > 1e-6) error stop "g"
    if (y(i) /= 4*a(i)*a(i)) error stop "y"
    do j = 1, m
        if (z(j, i) /= w(j, i)*(2*a(i) + 1)) error stop "z"
    end do
end do
print *, "ok"
end program
