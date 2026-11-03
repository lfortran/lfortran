module gpu_cuda_kernel_parameter_extents_m
    implicit none
    integer, parameter :: rows = 6
end module

program gpu_cuda_kernel_parameter_extents
    ! Bounds that are named constants are compile-time constants too
    ! (lfortran/lfortran#16569): a launch that is never split keeps them as
    ! constants in the kernel, which takes no bound arguments, as it does
    ! for literal bounds.
    use gpu_cuda_kernel_parameter_extents_m, only: rows
    implicit none
    integer, parameter :: n = 100, m = 30, lo = -3
    integer(8), parameter :: big = 5000000000_8
    integer :: i, j, s
    integer(8) :: l
    integer :: x(n), v(lo:m, rows)
    integer(8) :: w(2)
    ! A single loop.
    do concurrent (i = 1:n)
        x(i) = 2 * i
    end do
    ! A 2-D nest, with a constant expression and a bound from a module.
    do concurrent (i = lo:m, j = 1:rows)
        v(i, j) = i + j
    end do
    ! A reduction over a nest: no first-chunk flag and no bounds.
    s = 0
    do concurrent (i = lo + 1:m - 1, j = 2:rows) reduce(+:s)
        s = s + v(i, j)
    end do
    ! An integer(8) index with integer(4) and integer(8) named constants:
    ! the inlined bounds take the kind of the index.
    w = 0
    do concurrent (l = big - 1:big)
        w(l - big + 2) = l
    end do
    do concurrent (l = 1:n)
        x(l) = x(l) + 1
    end do
    print *, sum(x), s, w
end program
