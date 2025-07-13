program openmp_69
    use omp_lib
  implicit none
  integer :: flags(4)
  integer :: i

  flags = 0

  !$omp parallel num_threads(4) private(i)
  i = omp_get_thread_num()
  !$omp critical
  flags(i+1) = 1
  !$omp end critical
  !$omp end parallel

  do i = 1, 4
    if (flags(i) /= 1) then
      print *, 'Error: Thread ', i-1, ' did not execute!'
      error stop
    end if
  end do

  print *, 'Test passed: num_threads(', 4, ') used correctly.'
end program openmp_69
