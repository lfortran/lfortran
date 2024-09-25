subroutine operations(n, ctr, sum_v1, sum_v2,sum_v3)
    use omp_lib
    implicit none
    integer, intent(in) :: n
    double precision, intent(inout) :: ctr
    double precision, intent(inout) :: sum_v1, sum_v2, sum_v3
    
    integer :: local_ctr
    double precision :: local_sum_v1, local_sum_v2,local_sum_v3

    integer :: i

    local_ctr = 0
    local_sum_v1 = 0.0
    local_sum_v2 = 1.0
    local_sum_v3 = 0.0

    !$omp parallel private(i) reduction(+:local_ctr, local_sum_v1, local_sum_v3) reduction(*:local_sum_v2) 
    !$omp do
    do i = 1, n
        local_ctr = local_ctr + 1            ! Counting increment
        local_sum_v2 = local_sum_v2 * i      ! Sample multiplication operation for v2
        local_sum_v1 = local_sum_v1 + i      ! Sample sum operation for v1
        local_sum_v3 = local_sum_v3 - i      ! Sample subtraction operation for v3
    end do

    !$omp end do
    !$omp end parallel

    ctr = ctr + local_ctr
    sum_v1 = sum_v1 + local_sum_v1
    sum_v2 = sum_v2 * local_sum_v2  
    sum_v3 = sum_v3 + local_sum_v3
end subroutine

program openmp_35
    use omp_lib
    integer, parameter :: n = 16
    double precision :: ctr, sum_v1, sum_v2, sum_v3

    call omp_set_num_threads(8)
    ctr = 0.0
    sum_v1 = 0.0
    sum_v2 = 1.0  ! Initializing for multiplicative reduction
    sum_v3 = 0.0

    call operations(n, ctr, sum_v1, sum_v2,sum_v3)

    print *, "Total count: ", ctr
    print *, "Sum of v1: ", sum_v1
    print *, "Product result for v2: ", sum_v2
    print *, "Difference result for v3: ", sum_v3
end program