program doconcurrentloop_03
    integer :: verbose_levels, verbose_sum
    verbose_levels = 5
    verbose_sum = 0

    do concurrent ( integer :: i = 1 : verbose_levels )
        verbose_sum = verbose_sum + i
    end do

    print *, verbose_sum
    if (verbose_sum /= 15) error stop
end program