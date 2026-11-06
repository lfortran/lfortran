program gpu_metal_599
    ! Offloaded do concurrent loops with integer(8) indices. The kernel
    ! numbers the iterations in integer(8) from the device's thread and block
    ! built-ins, which are 32-bit: they must be widened before they are
    ! multiplied, or a launch of more than 2**32 threads wraps. The reference
    ! test of this file pins the widening, for the flat numbering and for the
    ! two-dimensional tiles; the run only checks the results of small loops.
    implicit none
    integer(8) :: i, j, n, m
    integer :: a(10), b(5, 7)
    n = 10
    m = 7
    a = 0
    do concurrent (i = 1:n)
        a(i) = int(2*i)
    end do
    print *, a
    if (any(a /= [(2*i, i = 1, 10)])) error stop 1
    b = 0
    do concurrent (j = 1:m, i = 1:5)
        b(i, j) = int(10*i + j)
    end do
    print *, b(1, 1), b(5, 7)
    do j = 1, m
        do i = 1, 5
            if (b(i, j) /= 10*i + j) error stop 2
        end do
    end do
end program
