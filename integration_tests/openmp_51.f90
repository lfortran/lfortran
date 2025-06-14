program openmp_51
    use omp_lib
    implicit none
    call omp_set_num_threads(10)

    !$omp parallel
        if(omp_get_thread_num() == 0) then
            !$omp task
                print *, "Task 0 done by TID:-",omp_get_thread_num()
            !$omp end task
        end if
        
        if(omp_get_thread_num() == 1) then
            !$omp task
                print *, "Task 1 done by TID:-",omp_get_thread_num()
            !$omp end task
        end if
    !$omp end parallel 
end program openmp_51