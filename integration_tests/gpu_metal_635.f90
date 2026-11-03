! SUM and MATMUL of a run-time sized array of the host inside a BLOCK in a
! DO CONCURRENT must leave the host's declaration of the array alone: the
! kernel bounds its loops with its own copy of the array's extents.
program gpu_metal_635
    implicit none
    real :: y(2), x(4,2), p(4,2), q(3,2)
    integer :: k, m

    ! Whole-array SUM of an automatic array of an enclosing BLOCK.
    k = 4
    block
        real :: w(k)
        w = 2.5
        do concurrent (m = 1:2)
            block
                y(m) = sum(w)
            end block
        end do
    end block
    if (any(y /= 10.0)) error stop

    ! SUM of an array expression and of a rank-2 array, with a local of
    ! the inner BLOCK.
    k = 3
    block
        real :: w(k), w2(k,k)
        w = 2.0
        w2 = 1.0
        do concurrent (m = 1:2)
            block
                real :: s
                s = sum(w * w) + sum(w2 * real(m))
                y(m) = s
            end block
        end do
    end block
    if (abs(y(1) - 21.0) > 1e-5 .or. abs(y(2) - 30.0) > 1e-5) error stop

    ! MATMUL of automatic arrays: its loops are bounded by their extents,
    ! and a temporary for a nested MATMUL is shaped by them.
    k = 3
    block
        real :: w(k), w2(k,k)
        w = [1.0, 2.0, 3.0]
        w2 = 1.0
        do concurrent (m = 1:2)
            block
                q(:,m) = matmul(w2, w) * real(m)
                y(m) = sum(matmul(w2, w))
            end block
        end do
    end block
    if (any(abs(q(:,1) - 6.0) > 1e-5)) error stop
    if (any(abs(q(:,2) - 12.0) > 1e-5)) error stop
    if (any(abs(y - 18.0) > 1e-5)) error stop

    ! Nested DO CONCURRENT with an automatic array of the outer BLOCK,
    ! read by an inner BLOCK through SUM of an array expression.
    x(:,1) = 1.0
    x(:,2) = 2.0
    call project(x, p)
    if (any(abs(p(:,1) - 10.0) > 1e-5)) error stop
    if (any(abs(p(:,2) - 20.0) > 1e-5)) error stop
    print *, p
contains
    subroutine project(x, y)
        real, intent(in) :: x(:,:)
        real, intent(out) :: y(:,:)
        integer :: k, n, d, m
        k = size(x, 1)
        do concurrent (n = 1:size(y, 1))
            block
                real :: weights(k)
                do concurrent (d = 1:k)
                    weights(d) = real(d)
                end do
                do concurrent (m = 1:size(y, 2))
                    block
                        real :: total
                        total = sum(weights * x(:,m))
                        y(n,m) = total
                    end block
                end do
            end block
        end do
    end subroutine
end program
