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

subroutine host_arrays(hs, gs, g43, g01, ws, ps, pt, pst)
implicit none
integer, intent(out) :: hs, gs, g43, g01, ws, ps, pt, pst
integer :: h(10), g(0:4, 3)
integer, allocatable :: w(:)
integer, target :: t(20)
integer, pointer :: p(:)
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
t = 0
p => t
call fill_pointer()
ps = sum(t)
t = 0
p => t(2:20:2)
call fill_pointer()
pt = 1000*sum(t(2:20:2)) + sum(t(1:19:2))
t = 0
call sections_pointer()
pst = 1000*sum(t) + 100*t(2) + 10*t(4) + t(20)
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

subroutine fill_pointer()
integer :: k
!$omp parallel do
do k = 1, size(p)
    p(k) = k
end do
!$omp end parallel do
end subroutine

subroutine sections_pointer()
!$omp parallel sections
!$omp section
p(1) = 5
!$omp section
p(10) = 6
!$omp end parallel sections
!$omp parallel
!$omp single
p(2) = 7
!$omp end single
!$omp end parallel
end subroutine
end subroutine
