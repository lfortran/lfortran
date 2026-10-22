program gpu_kernel_source_23
    ! A stencil whose outer loop index is the contiguous subscript of its
    ! stores, with one-sided rows at both ends of that index: the nest is
    ! split, but its interior is not unrolled and jammed along that index,
    ! which the threads of a launch run contiguously.
    implicit none
    integer, parameter :: n = 12, m = 5
    real :: a(n, m), b(n, m), e
    integer :: i, j
    do j = 1, m
        do i = 1, n
            a(i, j) = real(mod(i * 7 + j * 13, 17))
        end do
    end do
    do concurrent (i = 1:n, j = 1:m)
        if (i == 1 .or. i == n) then
            b(i, j) = a(i, j)
        else
            b(i, j) = a(i + 1, j) - a(i - 1, j)
        end if
    end do
    do j = 1, m
        do i = 1, n
            if (i == 1 .or. i == n) then
                e = a(i, j)
            else
                e = a(i + 1, j) - a(i - 1, j)
            end if
            if (abs(b(i, j) - e) > 1e-5) error stop 1
        end do
    end do
    print *, "ok"
end program
