program do_loop_04
    integer :: n = 2, k
    do k = 2, n
        print *, k
    end do

    ! without --use-loop-variable-after-loop

    ! remove/ update this test if we make using the loop variable after the loop by default
    if (k /= 2) error stop

    print *, "k after = ", k
end program
