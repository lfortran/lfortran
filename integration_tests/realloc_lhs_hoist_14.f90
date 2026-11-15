! A function that computes its result from its arguments alone changes
! nothing, whether it is declared PURE or not: calling it in a loop does not
! keep a reallocation of a variable that a procedure could reach, such as a
! module variable, in the loop. A function that changes that variable does.
module realloc_lhs_hoist_14_mod
implicit none
real, allocatable :: g(:)
contains

integer function wrap(i, n)
    integer, intent(in) :: i, n
    wrap = 1 + i - n*(i/n)
end function

! Reallocates `g` with one element less.
real function shrink()
    shrink = g(size(g))
    g = g(1:size(g) - 1)
end function

! The reallocation of `g` moves before the loop.
subroutine wrapped(a, n, s)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, intent(out) :: s
    integer :: i
    s = 0
    do i = 1, n
        g = a + 1
        s = s + g(wrap(i, size(g)))
    end do
end subroutine

! The reallocation of `g` stays: `shrink` changes its shape.
subroutine shrunk(a, n, s)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, intent(out) :: s
    integer :: i
    s = 0
    do i = 1, n
        g = a + 1
        s = s + shrink()
    end do
end subroutine

end module

program realloc_lhs_hoist_14
use realloc_lhs_hoist_14_mod
implicit none
real :: a(4), s

a = [1, 2, 3, 4]
call wrapped(a, 5, s)
! g = [2, 3, 4, 5], read at 2, 3, 4, 1, 2
if (abs(s - 17) > 1e-6) error stop 1
call shrunk(a, 3, s)
if (abs(s - 15) > 1e-6) error stop 2
if (size(g) /= 3) error stop 3
print *, "ok"
end program
