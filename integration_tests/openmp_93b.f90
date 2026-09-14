subroutine task_in_parallel(a, n)
implicit none
integer, intent(in) :: n
real, intent(inout) :: a(n)
!$omp parallel shared(a) num_threads(1)
!$omp task shared(a)
a(2) = a(2) + 5.0
!$omp end task
!$omp end parallel
!$omp parallel
!$omp single
!$omp task shared(a)
a(1) = a(1) + 10.0
!$omp end task
!$omp end single
!$omp end parallel
end subroutine

subroutine nested_parallel_do(a, n)
implicit none
integer, intent(in) :: n
real, intent(inout) :: a(n)
integer :: i
!$omp parallel shared(a) num_threads(1)
!$omp parallel do
do i = 1, n
    a(i) = a(i) + 1.0*i
end do
!$omp end parallel do
!$omp end parallel
end subroutine

subroutine fill_lbound(a, n)
implicit none
integer, intent(in) :: n
real, intent(inout) :: a(0:n-1)
integer :: i, m
m = n - 1
!$omp parallel do
do i = 0, m
    a(i) = 2.0*i
end do
!$omp end parallel do
end subroutine

subroutine sum_lbound(a, n, s)
implicit none
integer, intent(in) :: n
real, intent(in) :: a(0:n-1)
real, intent(out) :: s
integer :: i, m
m = n - 1
s = 0.0
!$omp parallel do reduction(+:s)
do i = 0, m
    s = s + a(i)
end do
!$omp end parallel do
end subroutine
