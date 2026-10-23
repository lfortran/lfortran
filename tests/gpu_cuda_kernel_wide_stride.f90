program gpu_cuda_kernel_wide_stride
    ! With --descriptor-index-64 an array whose first extent alone, 2**31 + 1,
    ! exceeds huge(0): the stride of its second dimension must be computed in
    ! 64 bits, or the second column wraps onto the first. Compile-only: the
    ! array takes 4 GB, which a run on the device (or its CPU emulation)
    ! copies there and back.
    implicit none
    logical(1), allocatable :: c(:,:)
    integer(8) :: k, n, l
    n = 2_8**31 + 1
    allocate(c(n, 2))
    c(1, 1) = .false.
    c(2, 1) = .false.
    c(n - 1, 2) = .false.
    c(n, 2) = .false.
    do concurrent (l = 1:2, k = n - 1:n)
        c(k, l) = .true.
    end do
    print *, c(n - 1, 1), c(n, 1), c(n - 1, 2), c(n, 2), c(1, 1), c(2, 1)
    if (.not. (c(n - 1, 1) .and. c(n, 1) .and. c(n - 1, 2) .and. c(n, 2))) &
        error stop 3
    if (c(1, 1) .or. c(2, 1)) error stop 4
    deallocate(c)
end program
