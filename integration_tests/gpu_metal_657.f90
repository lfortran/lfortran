! MATMUL in a DO CONCURRENT on sections with a step: each section is
! walked with its own step, whether it is an operand the loops run over,
! the other operand, the operand of a TRANSPOSE, the other operand of an
! enclosing + or -, or the target. Fixed and run-time steps, and a
! run-time sized dummy whose section of the target is the other operand.
module gpu_metal_657_m
    implicit none
contains
    subroutine mm_rev(k, a, w, v)
        integer, intent(in) :: k
        real, intent(in) :: a(k,k), w(k,2)
        real, intent(inout) :: v(k,2)
        integer :: m
        do concurrent (m = 1:2)
            v(:,m) = matmul(a, w(:,m)) + v(k:1:-1,m)
        end do
    end subroutine
end module

program gpu_metal_657
    use gpu_metal_657_m
    implicit none
    real :: v(3,2), w(3,2), a(3,3), b(3,3,2), c(3,3,2), r(3,3,2)
    real :: u(3,2), ru(3,2)
    integer :: m, i, j, k

    k = 3
    do j = 1, 3
        do i = 1, 3
            a(i,j) = real(i + 2*j)
        end do
    end do
    v(:,1) = [1.0, 2.0, 3.0]
    v(:,2) = [4.0, 5.0, 6.0]
    w(:,1) = [7.0, 8.0, 9.0]
    w(:,2) = [1.0, -2.0, 5.0]
    b = reshape([(real(i), i = 1, 18)], [3,3,2])

    ! The other operand of +, reversed.
    u = 0.0
    do concurrent (m = 1:2)
        u(:,m) = matmul(a, w(:,m)) + v(3:1:-1,m)
    end do
    do m = 1, 2
        ru(:,m) = matmul(a, w(:,m)) + v(3:1:-1,m)
    end do
    print *, u
    if (any(abs(u - ru) > 1e-4)) error stop

    ! The target, and the operand the loops run over, reversed.
    u = 0.0
    do concurrent (m = 1:2)
        u(3:1:-1,m) = matmul(a(3:1:-1,:), w(:,m))
    end do
    do m = 1, 2
        ru(3:1:-1,m) = matmul(a(3:1:-1,:), w(:,m))
    end do
    print *, u
    if (any(abs(u - ru) > 1e-4)) error stop

    ! Steps of 2 in both operands, and a run-time step in the target,
    ! the operands and the other operand of *.
    u = 0.0
    ru = 0.0
    do concurrent (m = 1:2)
        u(1:k:2,m) = matmul(a(1:k:2,1:k:2), w(1:k:2,m)) * v(k:1:-2,m)
    end do
    do m = 1, 2
        ru(1:k:2,m) = matmul(a(1:k:2,1:k:2), w(1:k:2,m)) * v(k:1:-2,m)
    end do
    print *, u
    if (any(abs(u - ru) > 1e-4)) error stop

    ! A vector on the left, a TRANSPOSE of a section with steps on the
    ! right.
    u = 0.0
    ru = 0.0
    do concurrent (m = 1:2)
        u(1:2,m) = matmul(w(1:3:2,m), transpose(a(1:3:2,3:1:-2))) &
            + v(3:1:-2,m)
    end do
    do m = 1, 2
        ru(1:2,m) = matmul(w(1:3:2,m), transpose(a(1:3:2,3:1:-2))) &
            + v(3:1:-2,m)
    end do
    print *, u
    if (any(abs(u - ru) > 1e-4)) error stop

    ! Two matrices, steps everywhere.
    c = b
    r = b
    do concurrent (m = 1:2)
        c(1:3:2,3:1:-1,m) = matmul(a(1:3:2,3:1:-1), &
            transpose(b(:,3:1:-1,m)))
    end do
    do m = 1, 2
        r(1:3:2,3:1:-1,m) = matmul(a(1:3:2,3:1:-1), &
            transpose(b(:,3:1:-1,m)))
    end do
    print *, c
    if (any(abs(c - r) > 1e-4)) error stop

    ! The target read reversed as the operand of the MATMUL.
    c = b
    r = b
    do concurrent (m = 1:2)
        c(:,:,m) = matmul(c(3:1:-1,:,m), a)
    end do
    do m = 1, 2
        r(:,:,m) = matmul(r(3:1:-1,:,m), a)
    end do
    print *, c
    if (any(abs(c - r) > 1e-4)) error stop

    ! A run-time sized dummy, the target reversed as the other operand.
    ru = v
    do m = 1, 2
        ru(:,m) = matmul(a, w(:,m)) + ru(3:1:-1,m)
    end do
    call mm_rev(3, a, w, v)
    print *, v
    if (any(abs(v - ru) > 1e-4)) error stop
end program
