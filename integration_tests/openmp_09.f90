subroutine parallel_factorial(n, ctr)
    use omp_lib
    use iso_fortran_env
    implicit none
    integer, intent(in) :: n
    integer(kind =int64), intent(inout) :: ctr
    
    integer(kind =int64) :: local_ctr
    
    integer :: i
    
    local_ctr = 1
    !$omp parallel private(i) reduction(*:local_ctr)
    !$omp do
    do i = 1, n   
        local_ctr = local_ctr * i
    end do
    !$omp end do
    !$omp end parallel
    
    ctr = ctr + local_ctr
    end subroutine
    
    program openmp_06
    use omp_lib
    use iso_fortran_env
    integer, parameter :: n = 20    
    integer(kind =int64) :: ctr
    integer(kind =int64) :: res 
    res =  2432902008176640000_int64 ! Factorial of 20
    
    call omp_set_num_threads(8)
    ctr = 0
    call parallel_factorial(n, ctr)
    
    print *, ctr
    if(ctr /= res) error stop
    end program