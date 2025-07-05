program openmp_60
  use omp_lib
  implicit none
integer :: sum , team_sums(4) = 0, local_sum=0
sum=0
!$omp teams num_teams(4) thread_limit(3) shared(team_sums) private(local_sum) reduction(+:sum)
!$omp parallel shared(team_sums) private(local_sum) reduction(+:sum)
  local_sum = omp_get_thread_num() * 10 + omp_get_team_num()
  sum = sum + local_sum
  !$omp critical
  team_sums(omp_get_team_num() + 1) = team_sums(omp_get_team_num() + 1) + local_sum
  !$omp end critical
  !$omp end parallel
!$omp end teams
  print*, team_sums
  print*,sum
  if(sum/=138) error stop
  if(team_sums(1) /= 30) error stop
  if(team_sums(2) /= 33) error stop
  if(team_sums(3) /= 36) error stop
  if(team_sums(4) /= 39) error stop
end program openmp_60