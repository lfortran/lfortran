program gpu_cuda_kernel_wide_stride
    ! With --descriptor-index-64 an array whose first extent alone, 2**31 + 1,
    ! exceeds huge(0): the stride of its second dimension must be computed in
    ! 64 bits, or the second column wraps onto the first. The same holds for
    ! the stride of the third dimension of an array whose first two extents
    ! fit in a default integer but whose product, 46341**2, does not.
    ! Compile-only: each array takes 4 GB, which a run on the device (or its
    ! CPU emulation) copies there and back.
    implicit none
    logical(1), allocatable :: c(:,:), e(:,:,:)
    integer(8) :: k, n, l
    integer :: j, m
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
    m = 46341
    allocate(e(m, m, 2))
    e(m, m, 1) = .false.
    e(m, m, 2) = .false.
    do concurrent (j = 1:2)
        e(m, m, j) = .true.
    end do
    print *, e(m, m, 1), e(m, m, 2)
    if (.not. (e(m, m, 1) .and. e(m, m, 2))) error stop 5
    deallocate(e)
end program
