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

subroutine host_arrays(hs, gs, g43, g01, ws)
implicit none
integer, intent(out) :: hs, gs, g43, g01, ws
integer :: h(10), g(0:4, 3)
integer, allocatable :: w(:)
h = 0
call fill_host()
hs = sum(h)
g = 0
call double_host()
gs = sum(g)
g43 = g(4, 3)
g01 = g(0, 1)
allocate(w(5))
w = 0
call fill_allocatable()
ws = sum(w)
contains
subroutine fill_host()
integer :: k
!$omp parallel do
do k = 1, 10
    h(k) = k
end do
!$omp end parallel do
end subroutine

subroutine double_host()
integer :: k
!$omp parallel do
do k = 0, 4
    g(k, 1) = 2*h(k + 1)
    g(k, 3) = 2*h(k + 6)
end do
!$omp end parallel do
end subroutine

subroutine fill_allocatable()
integer :: k
!$omp parallel do
do k = 1, 5
    w(k) = 2*k
end do
!$omp end parallel do
end subroutine
end subroutine
