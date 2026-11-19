! Assignments to arrays that are certainly unallocated where they execute,
! which allocate without testing the allocation or the shape of the target
! (allocate_unallocated): the result of a function and its allocatable
! components, INTENT(OUT) dummy arguments and their components, locals of
! a procedure or of a BLOCK, arrays just deallocated. Without --fast, an
! ALLOCATE of an allocated array is a run-time error, so a wrong proof
! would stop this program.
module realloc_lhs_unallocated_01_m
implicit none
type :: tensor_t
    real, allocatable :: values_(:)
end type
contains

pure function values(self) result(r)
    type(tensor_t), intent(in) :: self
    real, allocatable :: r(:)
    r = self%values_
end function

pure function tensor(v) result(t)
    real, intent(in) :: v(:)
    type(tensor_t) :: t
    t%values_ = v
end function

elemental function scaled(x, k) result(t)
    real, intent(in) :: x
    integer, intent(in) :: k
    type(tensor_t) :: t
    integer :: i
    t%values_ = [(x*i, i = 1, k)]
end function

! In a branch, after statements that do not name the result
pure function branches(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    integer :: m
    m = 2*n
    if (n > 2) then
        r = [n, m]
    else
        r = [m]
    end if
end function

recursive function countdown(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    if (n == 0) then
        r = [0]
    else
        r = [n, countdown(n - 1)]
    end if
end function

subroutine fill(x, t, n)
    integer, allocatable, intent(out) :: x(:)
    type(tensor_t), intent(out) :: t
    integer, intent(in) :: n
    integer :: i
    x = [(n, i = 1, n)]
    t%values_ = [real(n)]
end subroutine

subroutine locals(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    integer, allocatable :: a(:)
    integer :: i
    a = [(i, i = 1, n)]
    s = sum(a)
    do i = 1, 3
        block
            integer, allocatable :: b(:)
            integer :: j
            b = [(i, j = 1, n)]
            s = s + sum(b)
        end block
    end do
    deallocate(a)
    a = [1, 2, 3, 4, 5]
    s = s + sum(a)
    if (allocated(a)) deallocate(a)
    a = [10]
    s = s + sum(a)
end subroutine

end module

program realloc_lhs_unallocated_01
use realloc_lhs_unallocated_01_m
implicit none
type(tensor_t) :: t, u(3)
integer, allocatable :: x(:)
integer :: k, s

do k = 1, 3
    t = tensor([real(k), 2.0, 3.0])
    if (any(values(t) /= [real(k), 2.0, 3.0])) error stop 1
end do
u = scaled([1.0, 2.0, 3.0], 2)
if (any(u(3)%values_ /= [3.0, 6.0])) error stop 2
if (any(branches(3) /= [3, 6])) error stop 3
if (any(branches(1) /= [2])) error stop 4
if (any(countdown(3) /= [3, 2, 1, 0])) error stop 5
x = [1, 2, 3, 4, 5, 6, 7]
call fill(x, t, 2)
if (any(x /= [2, 2]) .or. any(t%values_ /= [2.0])) error stop 6
call fill(x, t, 3)
if (any(x /= [3, 3, 3]) .or. any(t%values_ /= [3.0])) error stop 7
call locals(4, s)
if (s /= 10 + 4 + 8 + 12 + 15 + 10) error stop 8
print *, "ok"
end program
