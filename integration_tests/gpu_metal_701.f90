program gpu_metal_701
    ! The single integer(8) loop -huge(0_8):huge(0_8) has 2**64 - 1
    ! iterations, which do not fit in integer(8): the offloaded launch must
    ! stop with an error (lfortran/lfortran#16569). Computed in integer(8),
    ! the extent end - start + 1 would wrap to -1 and the launch would be
    ! skipped, so without the check the program would run to the end and
    ! exit 0. The CPU would run the loop for ever, so this test runs only
    ! offloaded.
    implicit none
    integer :: a(1)
    integer(8) :: k, m
    m = huge(0_8)
    a = 0
    do concurrent (k = -m:m, k == m)
        a(1) = 1
    end do
    print *, a(1)
end program
