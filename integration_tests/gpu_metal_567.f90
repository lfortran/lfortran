program gpu_metal_567
    ! Offloaded loops that write arrays of more than huge(0) elements: the
    ! buffer's byte size and each element's offset must be counted in 64 bits,
    ! for an integer(8) subscript and for the product of an extent and a
    ! default-integer subscript alike, and a single extent of more than
    ! huge(0) must reach the kernel unwrapped. Compiled with
    ! --descriptor-index-64, the flag that selects 64-bit array indexing, which
    ! makes the kernel compute offsets in 64 bits. Only the elements the loops
    ! write are read, so the 2-4 GB arrays are mostly never touched.
    implicit none
    logical(1), allocatable :: a(:), b(:,:), c(:,:)
    integer(8) :: k, n, l
    integer :: i, j, m
    n = 2_8**31 + 10
    allocate(a(n))
    do concurrent (k = n - 2:n)
        a(k) = .true.
    end do
    print *, a(n - 2), a(n - 1), a(n)
    if (.not. (a(n - 2) .and. a(n - 1) .and. a(n))) error stop 1
    deallocate(a)
    ! 3*m = 2**31 + 1 elements
    m = 715827883
    allocate(b(3, m))
    do concurrent (j = m - 1:m, i = 1:3)
        b(i, j) = .true.
    end do
    print *, b(1, m - 1), b(3, m - 1), b(1, m), b(3, m)
    if (.not. (b(1, m - 1) .and. b(3, m - 1) .and. b(1, m) .and. b(3, m))) &
        error stop 2
    deallocate(b)
    ! The first extent alone, 2**31 + 1, exceeds huge(0): the stride of the
    ! second dimension must not wrap onto the first column.
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
