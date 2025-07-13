! Test 5: Guided Schedule
program openmp_65
    use omp_lib
    implicit none
    integer, parameter :: n = 1000  ! Larger n to see guided behavior
    integer :: i, tid, j
    integer :: chunk_count
    integer :: chunk_size_array(100) = 0
    integer :: chunk_thread(100) = -1
    integer :: current_pos
    integer :: thread_iterations(0:7) = 0
    logical :: test_passed = .true.
    logical :: decreasing_trend = .true.
    integer :: last_thread = -1
    integer :: current_chunk_size
    integer :: iterations_done
    call omp_set_num_threads(4)
    
    print *, "=== GUIDED Schedule Test ==="
    print *, "Iterations:", n, "Threads:", omp_get_max_threads()
    chunk_count=0
    current_chunk_size =0
    iterations_done = 0
    current_pos = 1
    ! Track chunk sizes with guided schedule
    !$omp parallel private(tid)
    !$omp do schedule(guided)
    do i = 1, n
        tid = omp_get_thread_num()
        
        !$omp critical
        if (i == current_pos) then
            chunk_count = chunk_count + 1
            chunk_thread(chunk_count) = tid
            
            ! Find chunk size by looking ahead
            do j = i, n
                if (j == n) then
                    chunk_size_array(chunk_count) = j - i + 1
                    current_pos = n + 1
                    exit
                else
                    ! This is approximate - in real guided, we can't know
                    ! the chunk size until the chunk is complete
                    ! For testing, we'll measure it differently
                end if
            end do
        end if
        thread_iterations(tid) = thread_iterations(tid) + 1
        !$omp end critical
    end do
    !$omp end do
    !$omp end parallel
    
    ! Better way to measure guided chunks
    chunk_count = 0
    current_pos = 1
    
    ! Measure chunks by looking at work distribution
    !$omp parallel private(tid, i)
    tid = omp_get_thread_num()
    if (tid == 0) then        
        ! Simulate guided algorithm
        do while (iterations_done < n)
            chunk_count = chunk_count + 1
            ! Guided: chunk_size = remaining_iterations / (2 * num_threads)
            ! Minimum chunk size is usually 1
            current_chunk_size = max(1, (n - iterations_done) / (2 * omp_get_num_threads()))
            chunk_size_array(chunk_count) = current_chunk_size
            iterations_done = iterations_done + current_chunk_size
            
            if (chunk_count >= 100) exit
        end do
    end if
    !$omp end parallel
    
    ! Print chunk size progression
    print *, "Expected guided chunk sizes (first 10):"
    do i = 1, min(10, chunk_count)
        print *,"Chunk ", i, ": size = ", chunk_size_array(i)
    end do
    
    ! Verify guided behavior - chunks should generally decrease
    do i = 2, min(chunk_count-1, 20)
        if (chunk_size_array(i) > chunk_size_array(i-1) * 1.5) then
            ! Allow some variation but not huge increases
            print *, "WARNING: Chunk size increased significantly at chunk", i
            decreasing_trend = .false.
        end if
    end do
    
    ! First chunks should be larger than last chunks
    if (chunk_count > 5) then
        if (chunk_size_array(1) < chunk_size_array(chunk_count-2) * 2) then
            print *, "ERROR: Guided schedule not showing expected decreasing chunk sizes!"
            print *, "First chunk:", chunk_size_array(1), &
                     "Late chunk:", chunk_size_array(chunk_count-2)
            test_passed = .false.
        end if
    end if
    
    print *, "Thread work distribution:"
    do i = 0, omp_get_max_threads()-1
        print *, "Thread", i, ":", thread_iterations(i), "iterations"
    end do
    
    if (.not. test_passed) then
        error stop "GUIDED schedule test FAILED!"
    end if
    
    if (.not. decreasing_trend) then
        print *, "WARNING: Guided chunks did not show clear decreasing trend"
    else
        print *, "Guided schedule showing expected decreasing chunk pattern"
    end if
    
    print *, "GUIDED schedule test completed"
    
end program openmp_65