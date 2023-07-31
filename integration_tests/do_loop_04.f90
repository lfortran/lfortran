program do_loop_04
    integer :: n = 2, k
    do k = 2, n
        print *, k
    end do

    ! without --use-loop-variable-after-loop

    if (k /= 2) error stop

    print *, "k after = ", k
end program
