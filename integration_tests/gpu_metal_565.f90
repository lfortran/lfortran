program gpu_metal_565
    ! A do concurrent nest of more than huge(0) iterations: the launch runs
    ! its 2**31 + 1 iterations in chunks of at most huge(0), which the
    ! kernel numbers in 32 bits; numbered in 32 bits as one launch, the last
    ! ones would get a negative flat index. Both extents are run-time values
    ! and the test compares with a copy of n so that neither the device
    ! compiler nor index-set splitting hides a wrapped index.
    implicit none
    integer :: c(3), i, j, n, m, k
    n = 715827883
    m = n
    k = 3
    c = 0
    do concurrent (j = 1:n, i = 1:k)
        if (j == m) c(i) = i
    end do
    print *, c
    if (c(1) /= 1 .or. c(2) /= 2 .or. c(3) /= 3) error stop 1
end program
