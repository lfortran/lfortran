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

subroutine task_pointers(tc, ye, ts, tz, tp)
implicit none
integer, intent(out) :: tc, ye, ts, tz, tp
integer, target :: t(20), y(4, 5), u(20), v(20), w(5)
integer, pointer :: pc(:), q(:, :), r(:, :), ps(:), pz(:), pp(:)
t = 0
w = 0
y = 0
u = 0
v = 0
pc => t
q => y(:, 2:4)
r => y(2:3, 5:5)
ps => u(3:3:2)
pz => v(5:4:2)
pp => t(1:20:2)
!$omp parallel
!$omp single
!$omp task shared(pc)
pc(1) = 3
pc(20) = 7
!$omp end task
!$omp task shared(q, r)
q(2, 2) = 9
r(2, 1) = 4
!$omp end task
!$omp task shared(ps)
ps(1) = 5
!$omp end task
!$omp task shared(pz)
pz = 1
!$omp end task
!$omp task private(pp) shared(w)
pp => w
pp(1) = 5
!$omp end task
!$omp end single
!$omp end parallel
tc = 100*t(1) + 10*t(20) + sum(t)
ye = 100*y(2, 3) + 10*y(3, 5) + sum(y)
ts = 10*u(3) + sum(u)
tz = sum(v)
tp = 10*w(1) + sum(w)
end subroutine
