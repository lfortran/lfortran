program openmp_59
  use omp_lib
  integer :: sum=0
  !$omp teams num_teams(3) reduction(+:sum)
    print*, omp_get_team_num()
    sum=sum+omp_get_team_num()
  !$omp end teams
  if(sum/=3) error stop
end program openmp_59