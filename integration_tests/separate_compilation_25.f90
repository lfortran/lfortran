program separate_compilation_25
  use stats_corr_separate_compilation_25, only: corr
  implicit none

  real :: y(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
  real :: result(2)

  result = corr(y)
  print *, result
  if (.not. all(result == [1.0, 4.0])) error stop
end program