module gpu_memory_04_mod
implicit none
integer, parameter :: n = 2**20
integer, allocatable :: g(:)
contains

    subroutine bump_global()
        g(n) = g(n) + 1
    end subroutine

    subroutine bump(a)
        integer, intent(inout) :: a(:)
        a(1) = a(1) + 1
    end subroutine

    subroutine fill(a, v)
        real, intent(out) :: a(:)
        real, intent(in) :: v
        a = v
    end subroutine

    ! Some work before the value is used, so that the kernel runs for a
    ! while after its launch returns; the result is `v` exactly.
    pure real function slow(v) result(r)
        real, intent(in) :: v
        integer :: k
        r = v
        do k = 1, 50
            r = 0.5*(r + v)
        end do
    end function

end module

program gpu_memory_04
! The host changes or frees an array that pending launches only read, in
! every way it can, with arrays large enough (4 MiB) for a runtime to hand
! them to the kernel in place (the managed memory model): by assignment,
! through a procedure (with the array as an argument, or as a module
! variable), through a pointer, by an internal read, by allocating it again,
! by deallocating it, and as a local of a `block` that ends.
use gpu_memory_04_mod
implicit none
integer, allocatable, target :: x(:)
integer, allocatable :: y(:)
real, allocatable :: r(:), s(:)
integer, pointer :: p(:)
integer :: i, it
character(len=8) :: text

allocate(x(n), y(n), g(n))
do i = 1, n
    x(i) = i
    g(i) = 0
end do

! A time loop that only reads x.
y = 0
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = y(i) + x(i)
    end do
end do
if (y(n) /= 3*n .or. y(1) /= 3) error stop 1

! Assignment between launches.
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = x(i)
    end do
    x(n) = x(n) + 1
end do
do concurrent (i = 1:n)
    y(i) = x(i)
end do
if (y(n) /= n + 3) error stop 2

! A procedure with the array as an argument.
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = x(i)
    end do
    call bump(x)
end do
do concurrent (i = 1:n)
    y(i) = x(i)
end do
if (y(1) /= 4) error stop 3

! A procedure that changes a module variable.
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = g(i)
    end do
    call bump_global()
end do
do concurrent (i = 1:n)
    y(i) = g(i)
end do
if (y(n) /= 3) error stop 4

! A pointer.
p => x
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = x(i)
    end do
    p(2) = p(2) + 10
end do
do concurrent (i = 1:n)
    y(i) = x(i)
end do
if (y(2) /= 32) error stop 5

! An internal read.
text = '77'
do concurrent (i = 1:n)
    y(i) = x(i)
end do
read(text, *) x(3)
do concurrent (i = 1:n)
    y(i) = x(i)
end do
if (y(3) /= 77) error stop 6

! Allocating it again, with other values.
do concurrent (i = 1:n)
    y(i) = x(i)
end do
deallocate(x)
allocate(x(n))
do i = 1, n
    x(i) = -i
end do
do concurrent (i = 1:n)
    y(i) = x(i)
end do
if (y(n) /= -n .or. y(1) /= -1) error stop 7

! A slow kernel, then a change by a procedure, by deallocation, and of a
! local of a block.
allocate(r(n), s(n))
do i = 1, n
    r(i) = i
end do
do concurrent (i = 1:n)
    s(i) = slow(r(i))
end do
call fill(r, -2.0)
if (s(n) /= n .or. s(1) /= 1) error stop 8
do concurrent (i = 1:n)
    s(i) = slow(r(i))
end do
deallocate(r)
if (s(n) /= -2) error stop 9
block
    real, allocatable :: w(:)
    allocate(w(n))
    do i = 1, n
        w(i) = 5*i
    end do
    do concurrent (i = 1:n)
        s(i) = slow(w(i))
    end do
end block
allocate(r(n))
r = -3
if (s(n) /= 5*real(n) .or. s(1) /= 5) error stop 10
print *, y(1), y(n), s(n)
end program
