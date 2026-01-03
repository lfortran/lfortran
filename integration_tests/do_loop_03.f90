program do_loop_03
    integer :: n = 2, k
    do k = 2, n
        print *, k
    end do

    ! with --use-loop-variable-after-loop

    if (k /= 3) error stop

    print *, "k after = ", k
end program
