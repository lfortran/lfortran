! An assignment in a DO CONCURRENT whose value calls MATMUL and also reads
! the target through a different section, outside the MATMUL, goes
! through a temporary of the iteration even when it has fixed sizes:
! MATMUL is lowered together with the rest of its statement, before the
! fixed-size temporaries are made, so without one it would write the
! target while still reading it.
program gpu_metal_658
    implicit none
    real :: v(3,2), w(3,2), a(3,3), b(3,3,2), c(3,3,2), r(3,3,2)
    real :: rv(3,2)
    integer :: m, i, j

    do j = 1, 3
        do i = 1, 3
            a(i,j) = real(i + 2*j)
        end do
    end do
    w(:,1) = [7.0, 8.0, 9.0]
    w(:,2) = [1.0, -2.0, 5.0]
    b = reshape([(real(i), i = 1, 18)], [3,3,2])

    ! The target reversed as the other operand of +.
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    rv = v
    do concurrent (m = 1:2)
        v(:,m) = matmul(a, w(:,m)) + v(3:1:-1,m)
    end do
    do m = 1, 2
        rv(:,m) = matmul(a, w(:,m)) + rv(3:1:-1,m)
    end do
    print *, v
    if (any(abs(v - rv) > 1e-4)) error stop

    ! The same, with the MATMUL inside a product.
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    rv = v
    do concurrent (m = 1:2)
        v(:,m) = 2.0 * matmul(a, w(:,m)) + v(3:1:-1,m)
    end do
    do m = 1, 2
        rv(:,m) = 2.0 * matmul(a, w(:,m)) + rv(3:1:-1,m)
    end do
    print *, v
    if (any(abs(v - rv) > 1e-4)) error stop

    ! Two matrices, the target's columns reversed.
    c = b
    r = b
    do concurrent (m = 1:2)
        c(:,:,m) = matmul(a, b(:,:,m)) + c(:,3:1:-1,m)
    end do
    do m = 1, 2
        r(:,:,m) = matmul(a, b(:,:,m)) + r(:,3:1:-1,m)
    end do
    print *, c
    if (any(abs(c - r) > 1e-4)) error stop
end program
