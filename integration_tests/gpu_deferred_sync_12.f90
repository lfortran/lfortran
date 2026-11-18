! A deferring twin is handed a derived-type actual argument without its
! caller waiting for the launches pending on it, as an array: the state of a
! model whose arrays the launches of each step use. The twin reads scalar
! components of its intent(in) argument and reads and writes those of the
! other on the host between its launches, which the launches do not touch.
! An intent(in) argument whose arrays a pending launch writes is waited for
! before the call.
module gpu_deferred_sync_12_m
implicit none
type :: layer
    integer, allocatable :: a(:)
    logical :: double
end type
type :: weights
    integer :: n
    type(layer), allocatable :: l(:)
end type
type :: state
    integer, allocatable :: x(:), y(:)
    integer :: steps = 0
end type
contains
    subroutine step(w, s, k)
        type(weights), intent(in) :: w
        type(state), intent(inout) :: s
        integer, intent(in) :: k
        integer :: l, i
        do l = 1, size(w%l)
            associate (y => w%l(l))
                if (y%double) then
                    do concurrent (i = 1:w%n)
                        s%x(i) = s%x(i) + 2*y%a(i) + k
                    end do
                else
                    do concurrent (i = 1:w%n)
                        s%x(i) = s%x(i) + y%a(i) - k
                    end do
                end if
                do concurrent (i = 1:w%n)
                    s%y(i) = s%y(i) + mod(s%x(i), 7)
                end do
            end associate
        end do
        s%steps = s%steps + 1
    end subroutine
end module

program gpu_deferred_sync_12
use gpu_deferred_sync_12_m
implicit none
integer, parameter :: n = 5000, layers = 3, steps = 20
type(weights) :: w
type(state) :: s
integer :: x(n), y(n), a(n, layers), i, l, k
w%n = n
allocate(w%l(layers), s%x(n), s%y(n))
do l = 1, layers
    allocate(w%l(l)%a(n))
    do i = 1, n
        w%l(l)%a(i) = mod(i*l, 11)
    end do
    w%l(l)%double = mod(l, 2) == 0
end do
s%x = 0
s%y = 0
do k = 1, steps
    call step(w, s, k)
    if (k == steps/2) then
        ! A launch writes the arrays of `w`, which the next call reads.
        do concurrent (i = 1:n)
            w%l(1)%a(i) = w%l(1)%a(i) + 1
        end do
    end if
end do
x = 0
y = 0
do l = 1, layers
    a(:, l) = [(mod(i*l, 11), i = 1, n)]
end do
do k = 1, steps
    do l = 1, layers
        if (mod(l, 2) == 0) then
            x = x + 2*a(:, l) + k
        else
            x = x + a(:, l) - k
        end if
        y = y + mod(x, 7)
    end do
    if (k == steps/2) a(:, 1) = a(:, 1) + 1
end do
if (s%steps /= steps) error stop 1
if (any(s%x /= x)) error stop 2
if (any(s%y /= y)) error stop 3
if (any(w%l(1)%a /= a(:, 1))) error stop 4
print *, sum(s%x), sum(s%y)
end program
