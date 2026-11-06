program doloop_23
! Unit-step DO loops with run-time bounds (lowered by the do_loops pass with
! an exit test LLVM can compute a trip count for under --fast) next to a
! non-unit and a run-time step (lowered as before).
implicit none
real :: x(8), y(8)
integer :: n, i, k
n = 8
do i = 1, n
    x(i) = i
    y(i) = 2
end do
if (abs(dot(n, x, y) - 72) > 1e-6) error stop 1
k = 0
do i = n, 1, -1
    k = k + i
end do
if (k /= 36) error stop 2
k = 0
do i = 1, n, 3
    k = k + i
end do
if (k /= 12) error stop 3
k = 0
do i = n, 1, -n/4
    k = k + i
end do
if (k /= 20) error stop 4
print *, "ok"
contains
real function dot(m, a, b) result(s)
    integer, intent(in) :: m
    real, intent(in) :: a(m), b(m)
    integer :: j
    s = 0
    do j = 1, m
        s = s + a(j)*b(j)
    end do
end function
end program
