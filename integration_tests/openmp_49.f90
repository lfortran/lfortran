program openmp_49
    use omp_lib
    implicit none
    call omp_set_num_threads(8)

    !$omp parallel
        !$omp master
            print *, "Welcome to Master Thread!"
            if(omp_get_thread_num() /= 0) error stop
        !$omp end master
    !$omp end parallel 
end program openmp_49