! A fixed-size array with unspecified locality that every iteration of a do
! concurrent loop defines whole before it references it is a temporary of the
! iteration: each GPU thread needs its own copy of it, not one shared buffer
! (issue #15307). An array whose elements the iterations define one each,
! or whose value is read after the loop, stays shared.
program gpu_metal_474
implicit none
integer, parameter :: n = 10000, m = 64
real :: g(m, n), t(m), y(n), u(4)
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
