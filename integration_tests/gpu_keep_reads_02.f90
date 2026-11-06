module gpu_keep_reads_02_mod
implicit none
integer, parameter :: n = 2**17
contains

    subroutine bump(a)
        integer, intent(inout) :: a(:)
        a(1) = a(1) + 1
    end subroutine

    ! A time loop whose host reads a sum at every iteration, in a procedure:
    ! its caller changes `x` between calls.
    integer function sums(x, nit) result(total)
        integer, intent(in) :: x(n), nit
        integer :: i, it, s
        total = 0
        do it = 1, nit
            s = 0
            do concurrent (i = 1:n) reduce(+:s)
                s = s + x(i)
            end do
            total = total + s
        end do
    end function

end module

program gpu_keep_reads_02
! Time loops whose host reads the result of a reduction at every iteration,
! and so waits there, while the arrays the launches read stay unchanged --
! until the host changes one, in every way it can: by assignment, through a
! procedure it calls (with the array as an argument, or one contained here
! that names it), through a pointer, by an internal read, by allocating it
! again, and between calls of a procedure. A runtime that keeps the copy of
! what a launch reads in device memory past such a wait (the copy model)
! must copy it again after each change. The arrays are larger than the size
! from which a runtime keeps such a copy.
use gpu_keep_reads_02_mod
implicit none
integer, allocatable, target :: x(:)
integer, pointer :: p(:)
integer :: i, it, s, total
character(len=8) :: text

allocate(x(n))
do i = 1, n
    x(i) = 1
end do

! Nothing changes x: the copy can be kept.
total = 0
do it = 1, 3
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
end do
if (total /= 3*n) error stop 1

! Assignment.
total = 0
do it = 1, 3
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
    x(n) = x(n) + 1
end do
! n, n + 1, n + 2
if (total /= 3*n + 3) error stop 2

! A procedure with the array as an argument.
total = 0
do it = 1, 3
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
    call bump(x)
end do
! x sums to n + 3, then n + 4, n + 5
if (total /= 3*n + 12) error stop 3

! A contained procedure that names it.
total = 0
do it = 1, 3
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
    call add_one()
end do
! n + 6, n + 7, n + 8
if (total /= 3*n + 21) error stop 4

! A pointer.
p => x
total = 0
do it = 1, 3
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
    p(2) = p(2) + 10
end do
! n + 9, n + 19, n + 29
if (total /= 3*n + 57) error stop 5

! An internal read.
total = 0
do it = 1, 2
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
    text = '0'
    read(text, *) x(3)
end do
! n + 39, then x(3) = 0: n + 38
if (total /= 2*n + 77) error stop 6

! Allocating it again, with other values.
total = 0
do it = 1, 2
    s = 0
    do concurrent (i = 1:n) reduce(+:s)
        s = s + x(i)
    end do
    total = total + s
    deallocate(x)
    allocate(x(n))
    do i = 1, n
        x(i) = 2
    end do
end do
! n + 38, then 2 n
if (total /= 3*n + 38) error stop 7

! Between calls of a procedure whose loop keeps the copy.
total = sums(x, 3)
x(5) = x(5) + 1
total = total + sums(x, 3)
if (total /= 12*n + 3) error stop 8
print *, total

contains

    subroutine add_one()
        x(4) = x(4) + 1
    end subroutine

end program
