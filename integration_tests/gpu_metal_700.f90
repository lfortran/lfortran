program gpu_metal_700
    ! An offloaded do concurrent nest whose number of iterations, the product
    ! of its extents, does not fit in integer(8) must stop the launch with
    ! an error (lfortran/lfortran#16569), not wrap the count and run some
    ! other number of iterations, or none. Each index here is a default
    ! integer and every extent, 2**32 - 1, fits in integer(8), but their
    ! product does not. The mask is true for one iteration only, so without
    ! the check the launch would be skipped or run without effect and the
    ! program would run to the end and exit 0. The CPU would run the nest
    ! for ever, so this test runs only offloaded.
    implicit none
    integer :: a(1), i, j, n
    n = huge(0)
    a = 0
    do concurrent (i = -n:n, j = -n:n, i == n .and. j == n)
        a(1) = 1
    end do
    print *, a(1)
end program
