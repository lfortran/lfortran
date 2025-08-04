
program openmp_55
    use omp_lib
    implicit none
    integer :: i,n=10,counter
    call omp_set_num_threads(8)
counter=0
  !$OMP PARALLEL 
    !$OMP MASTER
      do i = 1, n
          !$OMP TASK shared(counter)
                counter=counter+1
              print *, "Task Done by TID:-",omp_get_thread_num()
          !$OMP END TASK
      end do
    !$OMP END MASTER
  !$OMP END PARALLEL
      if(counter/=10) error stop

end program openmp_55