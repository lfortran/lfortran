! equivalent to openmp_09.f90

subroutine parallel_factorial(n, ctr)
    use omp_lib
    use iso_fortran_env
    implicit none
    integer, intent(in) :: n
    integer(kind =int64), intent(inout) :: ctr
    
    integer(kind =int64) :: local_ctr
    
    integer :: i
    
    local_ctr = 1

    do concurrent (i=1:n) reduce(*:local_ctr)
        local_ctr = local_ctr * i
    end do
    
    ctr = ctr + local_ctr
    end subroutine
    
    program do_concurrent_06
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
