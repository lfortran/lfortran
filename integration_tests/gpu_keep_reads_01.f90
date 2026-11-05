module gpu_keep_reads_01_mod
implicit none
integer, parameter :: n = 2**17
integer, allocatable :: g(:)
contains

    subroutine bump_global()
        g(n) = g(n) + 1
    end subroutine

    subroutine bump(a)
        integer, intent(inout) :: a(:)
        a(1) = a(1) + 1
    end subroutine

end module

program gpu_keep_reads_01
! The host changes an array that launches only read between two of them, in
! every way it can: by assignment, through a procedure (with the array as an
! argument, or as a module variable), through a pointer, by an internal
! read, and by allocating it again. A runtime that keeps the copy of what a
! launch reads in device memory until the next wait (the copy model) must
! copy it again after each change. The arrays are larger than the size from
! which a runtime keeps such a copy.
use gpu_keep_reads_01_mod
implicit none
integer, allocatable, target :: x(:)
integer, allocatable :: y(:)
integer, pointer :: p(:)
integer :: i, it
character(len=8) :: text

allocate(x(n), y(n), g(n))
do i = 1, n
    x(i) = i
    g(i) = 0
end do

! A time loop that only reads x: the copy can be kept.
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
print *, y(1), y(n)
end program
