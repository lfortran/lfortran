program openmp_67
    use omp_lib
    implicit none
    integer, parameter :: n = 10000
    integer :: i, j
    real :: static_time, dynamic_time, guided_time
    double precision :: start_time
    real :: a(n), b(n), c(n)
    
    call omp_set_num_threads(4)
    
    print *, "=== Schedule Comparison Test ==="
    print *, "Comparing performance of different schedules"
    print *, "Array size:", n, "Threads:", omp_get_max_threads()
    
    ! Initialize arrays
    ! Alternative: Initialize arrays with deterministic values
    do i = 1, n
        b(i) = real(i)
        c(i) = real(n - i + 1)
    end do
    
    ! Test STATIC
    start_time = omp_get_wtime()
    !$omp parallel do schedule(static)
    do i = 1, n
        a(i) = sqrt(b(i)) + log(abs(c(i)) + 1.0)
    end do
    !$omp end parallel do
    static_time = omp_get_wtime() - start_time
    
    ! Test DYNAMIC
    start_time = omp_get_wtime()
    !$omp parallel do schedule(dynamic)
    do i = 1, n
        a(i) = sqrt(b(i)) + log(abs(c(i)) + 1.0)
    end do
    !$omp end parallel do
    dynamic_time = omp_get_wtime() - start_time
    
    ! Test GUIDED
    start_time = omp_get_wtime()
    !$omp parallel do schedule(guided)
    do i = 1, n
        a(i) = sqrt(b(i)) + log(abs(c(i)) + 1.0)
    end do
    !$omp end parallel do
    guided_time = omp_get_wtime() - start_time
    
    ! Print results
    print '(A,F8.6,A)', "STATIC  time: ", static_time, " seconds"
    print '(A,F8.6,A)', "DYNAMIC time: ", dynamic_time, " seconds"
    print '(A,F8.6,A)', "GUIDED  time: ", guided_time, " seconds"
    
    ! Analysis
    print *, ""
    print *, "Analysis:"
    
    ! For uniform workload, static should be fastest
    if (static_time < dynamic_time * 0.9 .and. static_time < guided_time * 0.9) then
        print *, "✓ STATIC is fastest for uniform workload"
    else if (dynamic_time < static_time * 0.9) then
        print *, "! DYNAMIC is fastest"
    else if (guided_time < static_time * 0.9) then
        print *, "! GUIDED is fastest"
    else
        print *, "- All schedules perform similarly"
    end if
    
    ! Test with non-uniform workload
    print *, ""
    print *, "Testing with non-uniform workload..."
    
    ! Make some iterations heavier
    do i = 1, n
        if (mod(i, 10) == 0) then
            b(i) = b(i) * 1000.0  ! Make every 10th iteration heavier
        end if
    end do
    
    ! Test STATIC with non-uniform
    start_time = omp_get_wtime()
    !$omp parallel do schedule(static)
    do i = 1, n
        if (mod(i, 10) == 0) then
            ! Heavy computation
            a(i) = 0.0
            do j = 1, 100
                a(i) = a(i) + sqrt(b(i)) + log(abs(c(i)) + 1.0)
            end do
        else
            a(i) = sqrt(b(i)) + log(abs(c(i)) + 1.0)
        end if
    end do
    !$omp end parallel do
    static_time = omp_get_wtime() - start_time
    
    ! Test DYNAMIC with non-uniform
    start_time = omp_get_wtime()
    !$omp parallel do schedule(dynamic)
    do i = 1, n
        if (mod(i, 10) == 0) then
            a(i) = 0.0
            do j = 1, 100
                a(i) = a(i) + sqrt(b(i)) + log(abs(c(i)) + 1.0)
            end do
        else
            a(i) = sqrt(b(i)) + log(abs(c(i)) + 1.0)
        end if
    end do
    !$omp end parallel do
    dynamic_time = omp_get_wtime() - start_time
    
    print *, ""
    print *, "Non-uniform workload results:"
    print '(A,F8.6,A)', "STATIC  time: ", static_time, " seconds"
    print '(A,F8.6,A)', "DYNAMIC time: ", dynamic_time, " seconds"
    

    
end program openmp_67