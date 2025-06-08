program openmp_47
    use omp_lib
    implicit none
    real::res
    res=1
    call omp_set_num_threads(16)
    !$omp parallel reduction(*:res)
        res=res*1.5
    !$omp end parallel 
    if(res /= 1.5**16) error stop
    print *, res
end program openmp_47