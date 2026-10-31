program arrays_160
implicit none
integer :: a(-1:1), b(2, 3), lo, n, i, j
a = [10, 20, 30]
lo = -1
call change(lo, a)
if (lo /= 0) error stop 1
if (any(a /= [10, 77, 30])) error stop 2
lo = 4
call change(lo, a)
if (lo /= 5) error stop 3
if (any(a /= [10, 77, 30])) error stop 4
do j = 1, 3
    do i = 1, 2
        b(i, j) = 10 * i + j
    end do
end do
n = 3
call change_matrix(n, b)
if (n /= 1) error stop 5
if (b(2, 2) /= 99 .or. b(1, 3) /= 13) error stop 6
contains
subroutine change(lo, x)
integer, intent(inout) :: lo
integer, intent(inout) :: x(lo:lo+2)
integer :: entry_lo
entry_lo = lo
lo = lo + 1
if (lbound(x, 1) /= entry_lo) error stop 7
if (ubound(x, 1) /= entry_lo + 2) error stop 8
if (size(x + 1) /= 3) error stop 9
x(lo) = 77
end subroutine
subroutine change_matrix(n, x)
integer, intent(inout) :: n
integer, intent(inout) :: x(2, n)
n = 1
if (size(x, 2) /= 3 .or. size(x + 1) /= 6) error stop 10
x(2, 2) = 99
end subroutine
end program
