program openmp_66
  implicit none
  integer, parameter :: N = 100
  integer :: i, sum_expected, sum_actual

  sum_actual = 0
  sum_expected = (N*(N+1)) / 2  ! sum from 1 to N

  !$omp parallel do private(i)
  do i = 1, N
    !$omp atomic
    sum_actual = sum_actual + i
    !$omp end atomic
  end do
  !$omp end parallel do

  if (sum_actual /= sum_expected) then
    print *, 'Error: Incorrect result from atomic addition.'
    print *, 'Expected:', sum_expected, ' Got:', sum_actual
    error stop
  else
    print *, 'Test passed: atomic addition is correct. Sum =', sum_actual
  end if
end program openmp_66