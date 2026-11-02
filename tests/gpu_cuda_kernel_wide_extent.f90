program gpu_cuda_kernel_wide_extent
    ! One dimension of an offloaded do concurrent nest has more than
    ! huge(0) iterations, although each of its indices fits integer(4): the
    ! kernel has to compute the extent of that dimension, and each index
    ! from its ordinal, in integer(8). The bounds and the values compared
    ! with are run-time copies, so that neither the device compiler nor
    ! index-set splitting hides a wrapped index.
    implicit none
    integer :: c(4), i, j, n, lo, hi, a, b
    lo = -2000000000
    hi = 2000000000
    a = lo
    b = hi
    n = 2
    c = 0
    do concurrent (j = lo:hi, i = 1:n)
        if (j == a) c(i) = i
        if (j == b) c(n + i) = 10*i
    end do
    print *, c
    if (any(c /= [1, 2, 10, 20])) error stop 1
end program
