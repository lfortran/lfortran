program openmp_48
    use omp_lib
    implicit none
    integer :: i=0
    call omp_set_num_threads(8)

    !$omp parallel reduction(+:i)
        !$omp single
            i=i+1 !This will be done by one thread only hence 1 will be added to i
        !$omp end single
        i=i+1 !This will be done by all threads hence eventually 8 will be added to i
    !$omp end parallel 
    if(i/=9) error stop
end program openmp_48