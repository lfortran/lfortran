subroutine double_explicit(a, n)
implicit none
integer, intent(in) :: n
real, intent(inout) :: a(n)
integer :: i
!$omp parallel do
do i = 1, n
    a(i) = a(i)*2.0
end do
!$omp end parallel do
end subroutine

subroutine double_constant(a)
implicit none
real, intent(inout) :: a(100)
integer :: i
!$omp parallel do
do i = 1, 100
    a(i) = a(i)*2.0
end do
!$omp end parallel do
end subroutine

subroutine fill_2d(a, n, m)
implicit none
integer, intent(in) :: n, m
real, intent(inout) :: a(n, 0:m)
integer :: i, j
!$omp parallel do private(j)
do i = 1, n
    do j = 0, m
        a(i, j) = 10.0*i + j
    end do
end do
!$omp end parallel do
end subroutine

subroutine add_local(a, n)
implicit none
integer, intent(in) :: n
real, intent(inout) :: a(n)
real :: tmp(n)
integer :: i
!$omp parallel do
do i = 1, n
    tmp(i) = 1.0*i
end do
!$omp end parallel do
a = a + tmp
end subroutine
