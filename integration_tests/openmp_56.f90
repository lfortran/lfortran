program openmp_56
    use omp_lib
    implicit none
    integer :: counter
    counter=0
    call omp_set_num_threads(10)

    !$omp parallel
        !$omp task shared(counter)
            counter=counter+1
            print *, "Task done by TID:-",omp_get_thread_num()
        !$omp end task
    !$omp end parallel
    if(counter/=10) error stop
end program openmp_56