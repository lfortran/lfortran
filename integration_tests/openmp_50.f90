
program openmp_50
    use omp_lib
    implicit none
    integer :: i=0,n=10
    call omp_set_num_threads(8)

  !$OMP PARALLEL 
    !$OMP MASTER
      do i = 1, n
          !$OMP TASK  private(i)
              print *, "Task ",i,"done by TID:-",omp_get_thread_num()
          !$OMP END TASK
      end do
    !$OMP END MASTER
  !$OMP END PARALLEL

end program openmp_50
