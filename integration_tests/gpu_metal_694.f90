program gpu_metal_694
    ! matmul inside an offloaded loop is lowered to loops in the kernel,
    ! which run over the index space of one operand and turn it into the
    ! indices of the others. Compiled with --descriptor-index-64 the
    ! kernels compute element offsets in 64 bits, so these loops and
    ! indices have to count in integer(8) too (#16571): the lower bounds
    ! 2147483645, 3000000000 and 6442450941 do not fit integer(4). The
    ! arrays are small, so the test runs on the device (cuda_cpu in CI)
    ! and on the host.
    implicit none
    real, allocatable :: a(:,:), x(:,:), y(:,:), z(:,:), b(:,:), w(:,:)
    real, allocatable :: p(:,:,:), q(:,:,:), c(:,:,:)
    integer(8) :: lo8, los(4)
    integer :: i, j, k, l
    los = [1_8, 2147483645_8, 3000000000_8, 6442450941_8]
    do k = 1, 4
        lo8 = los(k)

        ! matrix times vector, the rows of the matrix counted from lo8,
        ! plus an array counted from one
        allocate(a(lo8:lo8 + 2, 3), x(3, 4), y(lo8:lo8 + 2, 4), z(3, 4))
        do i = 1, 3
            a(lo8 + i - 1, :) = [real(i), real(2*i), real(3*i)]
        end do
        do j = 1, 4
            x(:, j) = real(j)
            z(:, j) = real(100*j)
        end do
        y = 0
        do concurrent (j = 1:4)
            y(:, j) = matmul(a, x(:, j)) + z(:, j)
        end do
        print *, lo8, y(:, 4)
        do j = 1, 4
            do i = 1, 3
                if (abs(y(lo8 + i - 1, j) - (6*i*j + 100*j)) > 1e-3) &
                    error stop 1
            end do
        end do
        deallocate(a, x, y, z)

        ! vector times matrix, the columns of the matrix counted from
        ! lo8, the vector a strided section starting at lo8
        allocate(b(3, lo8:lo8 + 2), w(lo8:lo8 + 4, 4), y(lo8:lo8 + 2, 4))
        do i = 1, 3
            b(:, lo8 + i - 1) = [real(i), real(2*i), real(3*i)]
        end do
        do j = 1, 4
            w(:, j) = -1
            w(lo8:lo8 + 4:2, j) = real(j)
        end do
        y = 0
        do concurrent (j = 1:4)
            y(:, j) = matmul(w(lo8:lo8 + 4:2, j), b)
        end do
        print *, lo8, y(:, 4)
        do j = 1, 4
            do i = 1, 3
                if (abs(y(lo8 + i - 1, j) - 6*i*j) > 1e-3) error stop 2
            end do
        end do
        deallocate(b, w, y)

        ! matrix times matrix, every dimension counted from lo8
        allocate(p(lo8:lo8 + 1, lo8:lo8 + 1, 3), q(lo8:lo8 + 1, lo8:lo8 + 1, 3))
        allocate(c(lo8:lo8 + 1, lo8:lo8 + 1, 3))
        do l = 1, 3
            p(:, lo8, l) = [1.0, 2.0] * l
            p(:, lo8 + 1, l) = [3.0, 4.0] * l
            q(:, lo8, l) = [0.0, 1.0]
            q(:, lo8 + 1, l) = [1.0, 0.0]
        end do
        c = 0
        do concurrent (l = 1:3)
            c(:, :, l) = matmul(p(:, :, l), q(:, :, l))
        end do
        print *, lo8, c(:, :, 3)
        do l = 1, 3
            if (abs(c(lo8, lo8, l) - 3*l) > 1e-3) error stop 3
            if (abs(c(lo8 + 1, lo8, l) - 4*l) > 1e-3) error stop 4
            if (abs(c(lo8, lo8 + 1, l) - 1*l) > 1e-3) error stop 5
            if (abs(c(lo8 + 1, lo8 + 1, l) - 2*l) > 1e-3) error stop 6
        end do
        deallocate(p, q, c)
    end do
    print *, "ok"
end program
