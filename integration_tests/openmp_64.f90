program openmp_64
    use omp_lib
    implicit none
    integer, parameter :: n = 100
    integer :: i, tid, j
    real :: delay
    integer :: thread_iterations(0:7) = 0
    integer :: iteration_order(n)
    integer :: order_counter
    integer :: consecutive_count, max_consecutive
    real :: work_array(n)
    
    call omp_set_num_threads(4)
    order_counter = 0
    
    print *, "=== DYNAMIC Schedule Test ==="
    
    ! Dynamic schedule should NOT give contiguous blocks to threads
    !$omp parallel do schedule(dynamic, 1) private(tid, delay)
    do i = 1, n
        tid = omp_get_thread_num()
        
        ! Simulate varying work to see dynamic behavior
        if (mod(i, 10) == 0) then
            ! Some iterations take longer
            delay = 0.0
            do j = 1, 1000
                delay = delay + sin(real(j))
            end do
            work_array(i) = delay
        end if
        
        !$omp critical
        thread_iterations(tid) = thread_iterations(tid) + 1
        order_counter = order_counter + 1
        iteration_order(order_counter) = tid
        !$omp end critical
    end do
    !$omp end parallel do
    
    ! Analyze dynamic behavior
    print *, "Thread iteration counts:"
    do i = 0, omp_get_max_threads()-1
        print *, "Thread", i, ":", thread_iterations(i), "iterations"
    end do
    
    ! Check that work is not always in large contiguous blocks
    ! Dynamic with chunk=1 should interleave frequently
    max_consecutive = 0
    consecutive_count = 1
    
    do i = 2, n
        if (iteration_order(i) == iteration_order(i-1)) then
            consecutive_count = consecutive_count + 1
        else
            if (consecutive_count > max_consecutive) then
                max_consecutive = consecutive_count
            end if
            consecutive_count = 1
        end if
    end do
    
    print *, "Maximum consecutive iterations by same thread:", max_consecutive
    
    ! With dynamic(1), we shouldn't see very large consecutive blocks
    if (max_consecutive > 10) then
        print *, "WARNING: Dynamic schedule showing large consecutive blocks"
        print *, "This might indicate dynamic scheduling is not working properly"
    end if
    
    ! Check work distribution variation
    ! Dynamic should show more variation than static
    if (maxval(thread_iterations(0:3)) - minval(thread_iterations(0:3)) < 2) then
        print *, "WARNING: Work distribution too uniform for dynamic schedule"
    end if
    
    print *, "DYNAMIC schedule test completed"
    
end program openmp_64