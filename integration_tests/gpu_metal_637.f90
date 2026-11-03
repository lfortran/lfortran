! An assignment in a BLOCK in a DO CONCURRENT whose value reads the whole
! run-time sized array of the host it assigns, through a section, goes
! through a temporary of the same shape. The temporary takes its own copy
! of the array's extent, so that the host's declaration of the array is
! left alone when the kernel is built.
program gpu_metal_637
    implicit none
    integer :: k, m
    k = 4
    block
        real :: w(k)
        w = [1.0, 2.0, 3.0, 4.0]
        do concurrent (m = 1:1)
            block
                w = w(k:1:-1)
            end block
        end do
        print *, w
        if (any(w /= [4.0, 3.0, 2.0, 1.0])) error stop
    end block
end program
