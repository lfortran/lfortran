program gpu_metal_707
    ! The store steps through `i` and `j`, so the nest runs in tiles on
    ! CUDA, and the whole tiles of `i` and `j` alone come to 2**64 threads.
    ! The nest is empty, so the launch must not form its padded thread
    ! count: the empty `k` would only be multiplied in after the product
    ! overflowed integer(8) (lfortran/lfortran#16569). The ASR reference
    ! (tests.toml) pins that guard, as the wrapped product is 0 too. The
    ! empty dimension is the outermost, so the CPU runs no iteration either.
    implicit none
    integer :: b(4, 4), i, j, k, n, m
    n = huge(0)
    m = 0
    b = 7
    do concurrent (k = 1:m, i = -n:n, j = -n:n)
        b(i, j) = 0
    end do
    if (any(b /= 7)) error stop 1
    print *, "ok"
end program
