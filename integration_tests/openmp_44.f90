module openmp_44_parallel_sections
  implicit none

contains

  subroutine compute_a()
    print *, "Computing A"
  end subroutine compute_a

  subroutine compute_b()
    print *, "Computing B"
  end subroutine compute_b

  subroutine compute_c()
    print *, "Computing C"
  end subroutine compute_c

end module openmp_44_parallel_sections

program openmp_44
  use omp_lib
  use openmp_44_parallel_sections
  implicit none
  integer :: tid=0

  !$omp parallel sections reduction(+:tid)
  !$omp section
  call compute_a()
  tid = tid + omp_get_thread_num()
  print *, "Thread ID:", tid

  !$omp section
  call compute_b()
  tid = tid + omp_get_thread_num()
  print *, "Thread ID:", tid

  !$omp section
  call compute_c()
  tid = tid + omp_get_thread_num()
  print *, "Thread ID:", tid    
  !$omp end parallel sections
  print *, "Final Thread ID:", tid

end program openmp_44