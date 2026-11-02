program gpu_metal_602
    ! reduce(+:s) in offloaded loops compiled with --descriptor-index-64, so
    ! the kernels compute element offsets in 64 bits, also into the
    ! per-iteration buffers the reductions are accumulated in: an
    ! integer(8) sum over an integer(8) loop, and a real sum over a rank-2
    ! allocatable with lower bounds other than 1. Small, so it runs on the
    ! device (cuda_cpu in CI).
    implicit none
    integer(8) :: k, n, s
    real, allocatable :: x(:,:)
    real :: t
    integer :: i, j
    n = 1000
    s = 0
    do concurrent (k = 1:n) reduce(+:s)
        s = s + k
    end do
    print *, s
    if (s /= n*(n + 1)/2) error stop 1
    allocate(x(-1:3, 2:4))
    do j = 2, 4
        do i = -1, 3
            x(i, j) = i + 10*j
        end do
    end do
    t = 0
    do concurrent (j = 2:4, i = -1:3) reduce(+:t)
        t = t + x(i, j)
    end do
    print *, t
    if (abs(t - 465) > 1e-4) error stop 2
end program
