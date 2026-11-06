program gpu_metal_566
    ! A do concurrent nest of more than huge(0) iterations with literal
    ! bounds: its iteration count is then a product of constants, which must
    ! not be computed in 32 bits, or it wraps negative and every iteration
    ! is skipped. The launch runs it in chunks of at most huge(0)
    ! iterations, which the kernel numbers in 32 bits. The test
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
