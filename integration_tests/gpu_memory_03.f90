program gpu_memory_03
! Large arrays that launches only read, which a runtime may hand the kernel
! in place rather than copy at every launch, changed by the host between two
! launches that read them: the second launch must see the new values. The
! arrays are larger than what a runtime keeps in a pool of copies (32 MiB
! each), and are read whole, as sections at an offset (contiguous and not,
! of one-byte elements at an odd address), through a pointer, after being
! allocated again, and between launches that write an array the host reads.
implicit none
integer, parameter :: n = 2**23, rows = 2**10, cols = 2**13
real, allocatable, target :: w(:)
real, allocatable :: y(:), z(:), m(:, :), table(:, :), v(:)
real, pointer :: p(:)
integer(1), allocatable :: b(:)
integer, allocatable :: iy(:)
integer :: i, j, it

allocate(w(n), y(n))
do i = 1, n
    w(i) = i
end do

! The whole array, changed by assignment, element by element and whole.
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = 2*w(i)
    end do
    if (y(1) /= 2*(it - 1) + 2 .or. y(n) /= 2*real(n)) error stop 1
    w(1) = w(1) + 1
end do
w = -w
do concurrent (i = 1:n)
    y(i) = 2*w(i)
end do
if (y(1) /= -8 .or. y(n) /= -2*real(n) .or. y(77) /= -154) error stop 2

! Through a pointer to it, and a pointer to a section at an offset.
p => w
do it = 1, 3
    do concurrent (i = 1:n)
        y(i) = w(i)
    end do
    if (y(5) /= -5 - 10*(it - 1)) error stop 3
    p(5) = p(5) - 10
end do
p => w(1001:)
y = 0
call add_scaled(y(1:size(p)), p, 1.0)
if (y(1) /= -1001 .or. y(size(p)) /= -real(n)) error stop 4
p(1) = 3
call add_scaled(y(1:size(p)), p, 1.0)
if (y(1) /= -998 .or. y(2) /= -2004) error stop 5

! A contiguous column and a strided row of a large matrix.
allocate(m(rows, cols))
do j = 1, cols
    do i = 1, rows
        m(i, j) = i + rows*(j - 1)
    end do
end do
y = 0
call add_scaled(y(1:rows), m(:, 7), 1.0)
m(:, 7) = -m(:, 7)
call add_scaled(y(1:rows), m(:, 7), 2.0)
do i = 1, rows
    if (y(i) /= -(i + rows*6)) error stop 6
end do
y = 0
call add_scaled(y(1:cols), m(3, :), 1.0)
m(3, :) = 1
call add_scaled(y(1:cols), m(3, :), 1.0)
do j = 1, cols
    if (j /= 7 .and. y(j) /= 3 + rows*(j - 1) + 1) error stop 7
end do
if (y(7) /= -(3 + rows*6) + 1) error stop 8

! One column of a large table, as an embedding lookup, then another value
! of the same column.
allocate(table(rows, cols))
do j = 1, cols
    do i = 1, rows
        table(i, j) = i - j
    end do
end do
call gather_column(y(1:rows), table, 17)
if (y(1) /= -16 .or. y(rows) /= rows - 17) error stop 9
table(:, 17) = 5
call gather_column(y(1:rows), table, 17)
if (any(y(1:rows) /= 5)) error stop 10
call gather_column(y(1:rows), table, cols)
if (y(1) /= 1 - cols) error stop 11

! One-byte elements, from an odd offset.
allocate(b(4*n), iy(rows))
do i = 1, 4*n
    b(i) = int(modulo(i, 100), 1)
end do
call widen(iy, b(3:rows + 2))
if (iy(1) /= 3 .or. iy(rows) /= modulo(rows + 2, 100)) error stop 12
b(3) = -7
call widen(iy, b(3:rows + 2))
if (iy(1) /= -7 .or. iy(2) /= 4) error stop 13

! Freed and allocated again, perhaps at the same address, with other
! values; and moved to another variable.
deallocate(w)
allocate(w(n))
w = 7
do concurrent (i = 1:n)
    y(i) = 2*w(i)
end do
if (y(1) /= 14 .or. y(n) /= 14) error stop 14
call move_alloc(w, v)
v(n) = 1
do concurrent (i = 1:n)
    y(i) = 2*v(i)
end do
if (y(1) /= 14 .or. y(n) /= 2) error stop 15

! A launch writes what the host reads between launches, as a recurrent
! state the host checks after every step, while the array it reads stays.
allocate(z(n))
z = 0
do it = 1, 4
    do concurrent (i = 1:n)
        z(i) = 2*z(i) + v(i)
    end do
    if (z(1) /= 7*(2**it - 1) .or. z(n) /= 2**it - 1) error stop 16
end do
print *, "ok"

contains

    ! One column of a table, as an embedding lookup reads it.
    subroutine gather_column(y, table, col)
        real, intent(out) :: y(:)
        real, intent(in) :: table(:, :)
        integer, intent(in) :: col
        integer :: i
        do concurrent (i = 1:size(y))
            y(i) = table(i, col)
        end do
    end subroutine

    subroutine add_scaled(y, x, s)
        real, intent(inout) :: y(:)
        real, intent(in) :: x(:)
        real, intent(in) :: s
        integer :: i
        do concurrent (i = 1:size(y))
            y(i) = y(i) + s*x(i)
        end do
    end subroutine

    subroutine widen(y, b)
        integer, intent(out) :: y(:)
        integer(1), intent(in) :: b(:)
        integer :: i
        do concurrent (i = 1:size(y))
            y(i) = b(i)
        end do
    end subroutine

end program
