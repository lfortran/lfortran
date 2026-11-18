! The runtime replays a sequence of launches that repeats as CUDA graphs
! (lfortran_gpu_cuda.c), setting the arguments that change: a scalar that
! changes at every step, arrays that change from one phase to the next, and
! a number of iterations (the grid of a launch) that does too. The host
! reads and changes the arrays between steps now and then, which must come
! after the launches before it; a step that launches other kernels ends the
! sequence.
module gpu_graph_replay_01_m
implicit none
contains
    subroutine step(x, y, z, n, k)
        integer, intent(inout) :: x(:), y(:), z(:)
        integer, intent(in) :: n, k
        integer :: i
        do concurrent (i = 1:n)
            x(i) = x(i) + k
        end do
        do concurrent (i = 1:n)
            y(i) = mod(y(i) + 3*x(i), 1000)
        end do
        do concurrent (i = 1:n)
            z(i) = z(i) + mod(y(i), 7)
        end do
        do concurrent (i = 1:n/2)
            x(i) = x(i) - mod(z(i), 5)
        end do
        do concurrent (i = 1:n)
            y(i) = y(i) + 1
        end do
    end subroutine

    subroutine other(x, n)
        integer, intent(inout) :: x(:)
        integer, intent(in) :: n
        integer :: i
        do concurrent (i = 1:n)
            x(i) = x(i) * 2
        end do
    end subroutine

    ! The same on the host.
    subroutine step_host(x, y, z, n, k)
        integer, intent(inout) :: x(:), y(:), z(:)
        integer, intent(in) :: n, k
        x(1:n) = x(1:n) + k
        y(1:n) = mod(y(1:n) + 3*x(1:n), 1000)
        z(1:n) = z(1:n) + mod(y(1:n), 7)
        x(1:n/2) = x(1:n/2) - mod(z(1:n/2), 5)
        y(1:n) = y(1:n) + 1
    end subroutine
end module

program gpu_graph_replay_01
use gpu_graph_replay_01_m
implicit none
integer, parameter :: n = 20000
integer :: x(n), y(n), z(n), u(n), v(n), w(n)
integer :: a(n), b(n), c(n), ha(n), hb(n), hc(n)
integer :: k, i
x = [(mod(i, 13), i = 1, n)]
y = [(mod(i, 17), i = 1, n)]
z = 0
u = x; v = y; w = z
a = 1; b = 2; c = 3
ha = a; hb = b; hc = c
! The host does not wait for the steps: they are replayed as one graph.
! The number of iterations changes in the middle.
do k = 1, 100
    call step(x, y, z, iterations(k), k)
end do
do k = 1, 100
    call step_host(u, v, w, iterations(k), k)
end do
if (any(x /= u) .or. any(y /= v) .or. any(z /= w)) error stop 1
! The host reads and changes the arrays between some steps, and the steps
! change arrays.
do k = 101, 200
    if (k <= 130 .or. k > 160) then
        call step(x, y, z, n, k)
        call step_host(u, v, w, n, k)
    else
        call step(a, b, c, n, k)
        call step_host(ha, hb, hc, n, k)
    end if
    if (mod(k, 7) == 0) then
        if (x(7) /= u(7)) error stop 2
        y(5) = y(5) + 1
        v(5) = v(5) + 1
    end if
    if (k == 170) then
        ! Another sequence of launches ends the one that repeats.
        call other(z, n)
        w = w * 2
    end if
end do
if (any(x /= u)) error stop 3
if (any(y /= v)) error stop 4
if (any(z /= w)) error stop 5
if (any(a /= ha) .or. any(b /= hb) .or. any(c /= hc)) error stop 6
print *, sum(x), sum(y), sum(z)

contains

    integer function iterations(k)
        integer, intent(in) :: k
        iterations = n
        if (k > 50 .and. k <= 70) iterations = n/2 + mod(k, 3)
    end function
end program
