program gpu_cuda_kernel_wide_arrays
    ! Offloaded loops that write arrays of more than huge(0) elements: the
    ! buffer's byte size and each element's offset must be counted in 64 bits,
    ! for an integer(8) subscript and for the product of an extent and a
    ! default-integer subscript alike, and a single extent of more than
    ! huge(0) must reach the kernel unwrapped. Compiled with
    ! --descriptor-index-64, the flag that selects 64-bit array indexing, which
    ! makes the kernel compute offsets in 64 bits.
    !
    ! Compile-only: the gpu_cuda_kernel reference test of this file checks
    ! the 64-bit offsets in the kernel source. Each array takes 2 GB, which a
    ! run on the device, or its CPU emulation, copies there and back, and a
    ! run on the host would not execute any kernel. The kernel source of an
    ! array whose first extent alone exceeds huge(0), 4 GB with a second
    ! column, is checked by gpu_cuda_kernel_wide_stride.f90.
    implicit none
    logical(1), allocatable :: a(:), b(:,:)
    integer(8) :: k, n
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
end program
