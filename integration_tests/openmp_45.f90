module openmp_45_parallel_sections
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

end module openmp_45_parallel_sections

program openmp_45
  use omp_lib
  use openmp_45_parallel_sections
  implicit none
  integer :: tid=0

  !$omp parallel sections
  !$omp section
  call compute_a()

  !$omp section
  call compute_b()

  !$omp section
  call compute_c()
  !$omp end parallel sections

end program openmp_45