program openmp_default_03
implicit none
integer :: a(10), i
!$omp parallel do default(foo) shared(a)
do i = 1, 10
    a(i) = i
end do
!$omp end parallel do
end program
