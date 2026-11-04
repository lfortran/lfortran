program gpu_metal_703
    ! The constant-extent counterpart of gpu_metal_700: the bounds are
    ! literals, so the launch is decided at compile time
    ! (lfortran/lfortran#16569), but the product of the extents, each
    ! 2**32 - 1, still does not fit in integer(8) and the launch must stop
    ! with an error. The CPU would run the nest for ever, so this test runs
    ! only offloaded.
    implicit none
    integer :: a(1), i, j
    a = 0
    do concurrent (i = -2147483647:2147483647, j = -2147483647:2147483647, &
            i == 2147483647 .and. j == 2147483647)
        a(1) = 1
    end do
    print *, a(1)
end program
