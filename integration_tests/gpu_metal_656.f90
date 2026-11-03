! An assignment in a DO CONCURRENT whose target MATMUL reads goes through
! a temporary of the iteration: MATMUL is not elementwise, so writing the
! target while computing it would read elements already overwritten. The
! column on either side, run-time and fixed sizes, with and without a
! BLOCK around the assignment, extents that change from one iteration to
! the next, and many iterations at once. An operand that only reads the
! target's shape does not alias it.
module gpu_metal_656_m
    implicit none
contains
    subroutine mm(k, a, v)
        integer, intent(in) :: k
        real, intent(in) :: a(k,k)
        real, intent(inout) :: v(k,2)
        integer :: m
        do concurrent (m = 1:2)
            v(:,m) = matmul(a, v(:,m))
        end do
    end subroutine
end module

program gpu_metal_656
    use gpu_metal_656_m
    implicit none
    integer :: k, m, i, n, pair, nn(2)
    real :: v(3,2), a(3,3), b(3,3), out(64,3), w2(3,2), r(3,2)

    ! Fixed sizes, the column on the right and on the left.
    a = 1.0
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    do concurrent (m = 1:2)
        v(:,m) = matmul(a, v(:,m))
    end do
    print *, v
    if (any(abs(v(:,1) - 6.0) > 1e-5)) error stop
    if (any(abs(v(:,2) - 15.0) > 1e-5)) error stop
    do i = 1, 3
        b(i,:) = real(i)
    end do
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    do concurrent (m = 1:2)
        block
            v(:,m) = matmul(v(:,m), b)
        end block
    end do
    print *, v
    if (any(abs(v(:,1) - 14.0) > 1e-5)) error stop
    if (any(abs(v(:,2) - 32.0) > 1e-5)) error stop

    ! Run-time sizes: dummies, and automatic arrays of a BLOCK.
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    call mm(3, a, v)
    print *, v
    if (any(abs(v(:,1) - 6.0) > 1e-5)) error stop
    if (any(abs(v(:,2) - 15.0) > 1e-5)) error stop
    k = 3
    block
        real :: vb(k,2), ab(k,k)
        ab = 1.0
        vb(:,1) = [1.0, 2.0, 3.0]
        vb(:,2) = [4.0, 5.0, 6.0]
        do concurrent (m = 1:2)
            block
                vb(:,m) = matmul(ab, vb(:,m))
            end block
        end do
        print *, vb
        if (any(abs(vb(:,1) - 6.0) > 1e-5)) error stop
        if (any(abs(vb(:,2) - 15.0) > 1e-5)) error stop
    end block

    ! Only the shape of the target is read.
    w2(:,1) = [1.0, 2.0, 3.0]
    w2(:,2) = [4.0, 5.0, 6.0]
    v = 0.0
    do concurrent (m = 1:2)
        v(:,m) = matmul(a, w2(1:size(v,1), m))
    end do
    print *, v
    if (any(abs(v(:,1) - 6.0) > 1e-5)) error stop
    if (any(abs(v(:,2) - 15.0) > 1e-5)) error stop

    ! The extent of the section changes from one iteration to the next.
    nn = [2, 3]
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    r = v
    do m = 1, 2
        r(1:nn(m),m) = matmul(a(1:nn(m),1:nn(m)), r(1:nn(m),m))
    end do
    do concurrent (m = 1:2)
        v(1:nn(m),m) = matmul(a(1:nn(m),1:nn(m)), v(1:nn(m),m))
    end do
    print *, v
    if (any(abs(v - r) > 1e-5)) error stop

    ! A run-time section of a local of each iteration: the temporary is
    ! the iteration's own, not one buffer every thread writes.
    n = 3
    out = 0.0
    do concurrent (pair = 1:64)
        block
            real :: w(3)
            w = real(pair)
            w(1:n) = matmul(a(1:n,1:n), w(1:n))
            out(pair,:) = w
        end block
    end do
    do pair = 1, 64
        if (any(abs(out(pair,:) - 3.0 * real(pair)) > 1e-4)) error stop
    end do
    print *, out(64,:)
end program
