program gpu_metal_702
    ! A nest of three default integer loops of 2**21 iterations each has
    ! 2**63 iterations, one more than huge(0_8): the product overflows only
    ! at its last factor. The offloaded launch, which has a reduction, must
    ! stop with an error (lfortran/lfortran#16569). Computed in integer(8),
    ! the count would wrap to -huge(0_8) - 1 and the launch would be skipped,
    ! so without the check the program would print the sum it started with
    ! and exit 0. The CPU would run the nest for days, so this test runs only
    ! offloaded.
    implicit none
    integer :: i, j, k, n, s
    n = 2**21
    s = 0
    do concurrent (i = 1:n, j = 1:n, k = 1:n) reduce(+:s)
        if (i == n .and. j == n .and. k == n) s = s + 1
    end do
    print *, s
end program
