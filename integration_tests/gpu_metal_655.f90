! An assignment in a DO CONCURRENT whose target TRANSPOSE reads goes
! through a temporary of the iteration: TRANSPOSE is not elementwise, so
! writing the target while computing it would read elements already
! overwritten. Run-time and fixed sizes, with and without a BLOCK around
! the assignment.
module gpu_metal_655_m
    implicit none
contains
    subroutine tr(k, x)
        integer, intent(in) :: k
        real, intent(inout) :: x(k,k,2)
        integer :: m
        do concurrent (m = 1:2)
            x(:,:,m) = transpose(x(:,:,m))
        end do
    end subroutine
end module

program gpu_metal_655
    use gpu_metal_655_m
    implicit none
    integer :: k, m
    real :: w3(2,2,2)

    ! A whole run-time sized array of the host, in a BLOCK.
    k = 2
    block
        real :: w2(k,k)
        w2 = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
        do concurrent (m = 1:1)
            block
                w2 = transpose(w2)
            end block
        end do
        print *, w2
        if (any(w2 /= reshape([1.0, 3.0, 2.0, 4.0], [2, 2]))) error stop
    end block

    ! A fixed-size slab per iteration, with and without a BLOCK.
    w3(:,:,1) = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
    w3(:,:,2) = reshape([5.0, 6.0, 7.0, 8.0], [2, 2])
    do concurrent (m = 1:2)
        w3(:,:,m) = transpose(w3(:,:,m))
    end do
    print *, w3
    if (any(w3(:,:,1) /= reshape([1.0, 3.0, 2.0, 4.0], [2, 2]))) error stop
    if (any(w3(:,:,2) /= reshape([5.0, 7.0, 6.0, 8.0], [2, 2]))) error stop
    do concurrent (m = 1:2)
        block
            w3(:,:,m) = transpose(w3(:,:,m))
        end block
    end do
    print *, w3
    if (any(w3(:,:,1) /= reshape([1.0, 2.0, 3.0, 4.0], [2, 2]))) error stop
    if (any(w3(:,:,2) /= reshape([5.0, 6.0, 7.0, 8.0], [2, 2]))) error stop

    ! A run-time sized slab per iteration.
    call tr(2, w3)
    print *, w3
    if (any(w3(:,:,1) /= reshape([1.0, 3.0, 2.0, 4.0], [2, 2]))) error stop
    if (any(w3(:,:,2) /= reshape([5.0, 7.0, 6.0, 8.0], [2, 2]))) error stop
end program
