program gpu_metal_646
    ! A time-step loop swaps two pointers to arrays of derived type, so a
    ! DO CONCURRENT kernel writes through a pointer whose target is not
    ! known where the loop is offloaded. The steps need not wait for each
    ! other, but the host waits before it reads the result
    ! (lfortran/lfortran#16644). A block local with the `target` attribute
    ! that no pointer is associated with is not one the kernel may write.
    implicit none
    type :: cell_t
        real :: u
        real :: w
    end type
    integer, parameter :: n = 4096, nsteps = 50
    type(cell_t), target :: a(n), b(n)
    type(cell_t), pointer :: cur(:), nxt(:), tmp(:)
    integer :: i, step
    real :: s

    a%u = 1
    a%w = 0
    b%u = 0
    b%w = 0
    cur => a
    nxt => b
    do step = 1, nsteps
        do concurrent (i = 1:n)
            nxt(i)%u = cur(i)%u + 1
        end do
        tmp => cur
        cur => nxt
        nxt => tmp
    end do
    if (abs(cur(1)%u - real(nsteps + 1)) > 1.0e-3) error stop 1
    if (abs(cur(n)%u - real(nsteps + 1)) > 1.0e-3) error stop 2
    if (abs(nxt(n)%u - real(nsteps)) > 1.0e-3) error stop 3

    s = 0
    do step = 1, nsteps
        s = s + 1
        block
            real, target :: scratch(4)
            scratch = s
            do concurrent (i = 1:n)
                nxt(i)%u = cur(i)%u + 1
                nxt(i)%w = cur(i)%w + 2
            end do
            if (any(scratch /= s)) error stop 4
        end block
        tmp => cur
        cur => nxt
        nxt => tmp
    end do
    if (abs(cur(1)%u - real(2 * nsteps + 1)) > 1.0e-3) error stop 5
    if (abs(cur(n)%w - real(2 * nsteps)) > 1.0e-3) error stop 6
    print *, cur(1)%u, cur(n)%w
end program
