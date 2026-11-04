program gpu_metal_705
    ! The constant-bound counterpart of gpu_metal_701: the single integer(8)
    ! loop -huge(0_8):huge(0_8) with literal bounds has 2**64 - 1
    ! iterations, which do not fit in integer(8), so the offloaded launch
    ! must stop with an error (lfortran/lfortran#16569). The CPU would run
    ! the loop for ever, so this test runs only offloaded.
    implicit none
    integer :: a(1)
    integer(8) :: k
    a = 0
    do concurrent (k = -huge(0_8):huge(0_8), k == 9223372036854775807_8)
        a(1) = 1
    end do
    print *, a(1)
end program
