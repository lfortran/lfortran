program do_loop_04
    integer :: n = 2, k
    do k = 2, n
        print *, k
    end do

    ! Per Fortran standard, loop variable gets one step beyond final value
    if (k /= 3) error stop

    print *, "k after = ", k
end program
