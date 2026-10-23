program gpu_metal_566
    ! A do concurrent nest of more than huge(0) iterations with literal
    ! bounds: the offloaded kernel's iteration count is then a product of
    ! integer(8) constants, which the kernel source must not compute in C
    ! int, or it wraps negative and every iteration is skipped. The test
    ! compares with a run-time copy of the bound so that index-set splitting
    ! does not take the loop apart.
    implicit none
    integer :: c(3), i, j, m
    m = 715827883
    c = 0
    do concurrent (j = 1:715827883, i = 1:3)
        if (j == m) c(i) = i
    end do
    print *, c
    if (c(1) /= 1 .or. c(2) /= 2 .or. c(3) /= 3) error stop 1
end program
