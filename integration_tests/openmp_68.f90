program openmp_68
    use omp_lib
    implicit none
    integer, parameter :: n = 100
    integer :: i, tid, j
    integer :: thread_chunks(0:7) = 0
    integer :: chunk_sizes(100)  ! Track size of each chunk given out
    integer :: chunk_count
    integer :: current_iteration
    integer :: chunk_start(100), chunk_thread(100)
    logical :: test_passed = .true.
    logical :: looks_like_static = .true.
    current_iteration = 1
    chunk_count = 0
    call omp_set_num_threads(4)
    
    print *, "=== DYNAMIC Schedule with chunk=", 5, "==="
    
    ! Track chunks
    !$omp parallel private(tid)
    !$omp do schedule(dynamic, 5)
    do i = 1, n
        tid = omp_get_thread_num()
        
        !$omp critical
        ! Check if this is start of new chunk
        if (i == current_iteration) then
            chunk_count = chunk_count + 1
            chunk_start(chunk_count) = i
            chunk_thread(chunk_count) = tid
            thread_chunks(tid) = thread_chunks(tid) + 1
            
            ! Find chunk size
            if (i + 5 - 1 <= n) then
                chunk_sizes(chunk_count) = 5
                current_iteration = i + 5
            else
                chunk_sizes(chunk_count) = n - i + 1
                current_iteration = n + 1
            end if
        end if
        !$omp end critical
    end do
    !$omp end do
    !$omp end parallel
    
    ! Verify dynamic chunk behavior
    print *, "Total chunks distributed:", chunk_count
    print *, "Chunks per thread:"
    do i = 0, omp_get_max_threads()-1
        print *, "Thread", i, ":", thread_chunks(i), "chunks"
    end do
    
    ! Check chunk sizes
    do i = 1, chunk_count-1
        if (chunk_sizes(i) /= 5) then
            print *, "ERROR: Chunk", i, "has size", chunk_sizes(i), "expected", 5
            test_passed = .false.
        end if
    end do
    
    ! Last chunk might be smaller
    if (chunk_sizes(chunk_count) > 5) then
        print *, "ERROR: Last chunk too large!"
        test_passed = .false.
    end if
    
    ! With dynamic, chunks should not follow a strict pattern
    ! Check for round-robin pattern (which would indicate static behavior)
    
    do i = 2, min(chunk_count, 8)
        if (chunk_thread(i) /= mod(chunk_thread(1) + i - 1, omp_get_max_threads())) then
            looks_like_static = .false.
            exit
        end if
    end do
    
    if (looks_like_static .and. chunk_count > 4) then
        print *, "WARNING: Dynamic schedule showing static-like round-robin pattern!"
    end if
    
    if (.not. test_passed) then
        error stop "DYNAMIC chunk schedule test FAILED!"
    end if
    print *, "DYNAMIC chunk schedule test completed"
    
end program openmp_68