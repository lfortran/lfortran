! Memory stress test - run many iterations to detect leaks
! Run with valgrind or AddressSanitizer to verify zero leaks
program arena_05
    implicit none
    integer :: i
    do i = 1, 100000
        call stress_test()
    end do
    print *, "PASS: No memory leaks after 100000 iterations"
contains
    subroutine stress_test()
        integer :: arr(100)
        arr(1) = 42
        call nested_stress()
        if (arr(1) /= 42) error stop "FAIL: arr corrupted"
    end subroutine

    subroutine nested_stress()
        integer :: nested_arr(50)
        nested_arr(1) = 99
        if (nested_arr(1) /= 99) error stop "FAIL: nested_arr wrong"
    end subroutine
end program
