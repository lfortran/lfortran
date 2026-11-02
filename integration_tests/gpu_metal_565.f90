program gpu_metal_565
    ! A do concurrent nest of more than huge(0) iterations: the offloaded
    ! kernel has to number its 2**31 + 1 iterations in integer(8), or the
    ! last ones get a negative flat index. Both extents are run-time values
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
