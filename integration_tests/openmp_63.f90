program openmp_63
    use omp_lib
    implicit none
    integer, parameter :: n = 100
    integer, parameter :: max_threads = 8
    integer :: i, tid, nthreads
    integer :: thread_iterations(1:max_threads) = 0  ! Count iterations per thread
    integer :: thread_first(1:max_threads) = 1000    ! First iteration for each thread
    integer :: thread_last(1:max_threads) = -1       ! Last iteration for each thread
    integer :: expected_chunk_size
    logical :: test_passed = .true.
    
    ! Set number of threads
    call omp_set_num_threads(4)
    nthreads=0
    
    !$omp parallel private(tid)
    !$omp single
    nthreads = omp_get_num_threads()
    !$omp end single
    !$omp end parallel
    print *, "Testing STATIC schedule with", nthreads, "threads"
    
    ! Test static schedule (default chunk)
    !$omp parallel do schedule(static) private(tid)
    do i = 1, n
        tid = omp_get_thread_num() + 1
        !$omp critical
        thread_iterations(tid) = thread_iterations(tid) + 1
        if (i < thread_first(tid)) thread_first(tid) = i
        if (i > thread_last(tid)) thread_last(tid) = i
        !$omp end critical
    end do
    !$omp end parallel do

    print*, thread_first(1:nthreads)
    print*, thread_last(1:nthreads)
    print*, thread_iterations(1:nthreads)
    
    ! Verify static behavior
    print *, "=== STATIC Schedule Results ==="
    expected_chunk_size = (n + nthreads - 1) / nthreads
    
    do i = 1, nthreads
        print '(A,I1,A,I3,A,I3,A,I3)', &
            "Thread ", i, ": iterations=", thread_iterations(i), &
            ", first=", thread_first(i), ", last=", thread_last(i)
        
        ! Static should give contiguous chunks
        if (thread_iterations(i) > 0) then
            ! Check contiguous iterations
            if (thread_last(i) - thread_first(i) + 1 /= thread_iterations(i)) then
                print *, "ERROR: Thread", i, "did not get contiguous iterations!"
                test_passed = .false.
            end if
            
            ! Check chunk sizes are roughly equal
            if (abs(thread_iterations(i) - expected_chunk_size) > 1) then
                print *, "ERROR: Thread", i, "chunk size deviates too much!"
                test_passed = .false.
            end if
        end if
    end do
    
    if (.not. test_passed) then
        error stop "STATIC schedule test FAILED!"
    end if
    print *, "STATIC schedule test PASSED!"
    
end program openmp_63