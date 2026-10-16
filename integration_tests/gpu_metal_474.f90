! An array with unspecified locality that every iteration of a do concurrent
! loop defines whole before it references it is a temporary of the
! iteration: each GPU thread needs its own copy of it, not one shared buffer
! (issue #15307). An array whose elements the iterations define one each,
! or whose value is read after the loop, stays shared.
module gpu_metal_474_m
implicit none
contains
    subroutine reverse_columns(m, n, x, g)
        integer, intent(in) :: m, n
        real, intent(in) :: x(m, n)
        real, intent(out) :: g(m, n)
        real :: w(m)
        integer :: j
        do concurrent (j = 1:n)
            w = 2 * x(:, j)
            g(:, j) = w(m:1:-1)
        end do
    end subroutine
end module

program gpu_metal_474
use gpu_metal_474_m
implicit none
integer, parameter :: n = 10000, m = 64
real :: g(m, n), t(m), y(n), u(4)
real, allocatable :: x(:, :), h(:, :)
integer :: it, j, k

! A fixed-size temporary, defined element by element, in a loop that
! runs twice.
do it = 1, 2
    do concurrent (j = 1:n)
        do k = 1, m
            t(k) = j + k + it
        end do
        do k = 1, m
            g(k, j) = t(m + 1 - k)
        end do
    end do
end do
do j = 1, n
    do k = 1, m
        if (g(k, j) /= j + m + 1 - k + 2) error stop 1
    end do
end do
! The temporary is defined again before it is read after the loop.
t = 1
if (sum(t) /= m) error stop 2

! A run-time sized temporary, defined by a whole-array assignment.
allocate(x(32, 4096), h(32, 4096))
do j = 1, 4096
    do k = 1, 32
        x(k, j) = k + 100 * j
    end do
end do
call reverse_columns(32, 4096, x, h)
do j = 1, 4096
    do k = 1, 32
        if (h(k, j) /= 2 * (33 - k + 100 * j)) error stop 3
    end do
end do

! Distinct elements per iteration: one shared array, read after the loop.
do concurrent (j = 1:n)
    y(j) = 3 * j
end do
do j = 1, n
    if (y(j) /= 3 * j) error stop 4
end do

! A temporary read after the loop keeps the value the loop gave it.
u = 0
do concurrent (j = 1:1)
    u = [1., 2., 3., 4.] * j
    y(j) = sum(u)
end do
if (any(u /= [1, 2, 3, 4])) error stop 5
if (y(1) /= 10) error stop 6
print *, "ok"
end program
