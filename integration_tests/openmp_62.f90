program openmp_62
    use omp_lib
    implicit none
integer :: array(1000), i, j, sum=0
array(1)=3
!$omp teams num_teams(2) thread_limit(5)
!$omp distribute
do i = 1, 1000, 100
    print*,omp_get_num_threads(), omp_get_max_threads()
  !$omp parallel do
  do j = i, min(i+99, 1000)
    array(j) = j * 3
  end do
  !$omp end parallel do
end do
!$omp end distribute
!$omp end teams

! Sum of all elements
!$omp parallel do reduction(+:sum)
do i=1,1000
sum=sum+array(i)
end do
!$omp end parallel do

print*, sum
if(sum/=1501500) error stop
end program openmp_62