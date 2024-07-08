! equivalent to openmp_08.f90

subroutine increment_ctr(n, ctr)
    use omp_lib
    implicit none
    integer, intent(in) :: n
    real, intent(out) :: ctr
    
    real :: local_ctr
    
    integer :: i
    
    local_ctr = 1

    do concurrent (i=1:n) reduce(*:local_ctr)
        local_ctr = local_ctr * 1.5
    end do
    
    ctr = ctr + local_ctr
end subroutine
    
program do_concurrent_05
    use omp_lib
    integer, parameter :: n = 10
    real :: ctr
    real :: res = 1.5**10
    
    call omp_set_num_threads(8)
    ctr = 0
    call increment_ctr(n, ctr)
    print *, ctr
    if(abs((ctr - res)) > 0.0002 ) error stop
end program
