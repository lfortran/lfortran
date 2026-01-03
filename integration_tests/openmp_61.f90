program openmp_61
    use omp_lib
    implicit none
integer :: array(1005), i,sum=0
!$omp teams num_teams(4)
!$omp distribute
do i = 1, 1000
  array(i) = i * 2
end do
!$omp end distribute
!$omp end teams

! Sum of all elements
!$omp parallel do reduction(+:sum)
do i=1,1000
sum=sum+array(i)
end do
!$omp end parallel do

print *,sum
if(sum/=1001000) error stop
end program openmp_61