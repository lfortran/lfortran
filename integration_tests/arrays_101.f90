program static_large_array
    implicit none
    ! Test that large fixed-size arrays use heap allocation instead of stack
    ! This prevents stack overflow for arrays >= 4096 bytes (4 KB)
    ! The allocated memory is automatically freed at function exit

    ! Small array (4 * 100 = 400 bytes < 4096) - should use stack
    real :: small_arr(100)

    ! Large array (4 * 2000 = 8000 bytes >= 4096) - should use heap
    real :: large_arr(2000)

    integer :: i
    real :: sum

    ! Initialize arrays
    small_arr = 1.0
    large_arr = 2.0

    ! Simple verification
    if (small_arr(1) /= 1.0) error stop
    if (large_arr(1) /= 2.0) error stop

    ! Test BLOCK in loop - arrays must be freed each iteration
    ! Without heap allocation, this would cause stack overflow or memory leak
    sum = 0.0
    do i = 1, 1000
        block
            real :: block_arr(500)  ! 2000 bytes, but BLOCK always uses heap
            block_arr = real(i)
            sum = sum + block_arr(1)
        end block
    end do
    if (abs(sum - 500500.0) > 0.1) error stop

    print *, "OK"
end program
