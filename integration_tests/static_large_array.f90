program static_large_array
    implicit none
    ! Test that large fixed-size arrays use heap allocation instead of stack
    ! This prevents stack overflow for arrays > 65536 bytes (64 KB)
    ! The allocated memory is automatically freed at function exit

    ! Small array (4 * 100 = 400 bytes) - should use stack
    real :: small_arr(100)

    ! Large array (4 * 20000 = 80000 bytes > 65536) - should use heap
    real :: large_arr(20000)

    ! Initialize arrays
    small_arr = 1.0
    large_arr = 2.0

    ! Simple verification
    if (small_arr(1) /= 1.0) error stop
    if (large_arr(1) /= 2.0) error stop

    print *, "OK"
end program
