! The allocatable array result of a function is moved into the variable it
! is assigned to, unless the variable is allocated with its shape and keeps
! its bounds or its storage (F2018 10.2.1.3): for numbers, and now also for
! a derived type with allocatable components that is not finalized, also
! when the variable is the copy an ASSOCIATE makes of the result. A move
! does not reallocate the variable first.
module realloc_lhs_move_01_m
implicit none
type :: tensor_t
    real, allocatable :: values_(:)
end type
contains

pure function tensors(n) result(t)
    integer, intent(in) :: n
    type(tensor_t), allocatable :: t(:)
    integer :: i
    allocate(t(n))
    do i = 1, n
        t(i)%values_ = [real(i), real(2*i)]
    end do
end function

pure function numbers(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    integer :: i
    r = [(real(i), i = 1, n)]
end function

end module

program realloc_lhs_move_01
use realloc_lhs_move_01_m
implicit none
type(tensor_t), allocatable :: a(:)
type(tensor_t), allocatable, target :: b(:)
type(tensor_t), pointer :: p
real, allocatable :: x(:)
integer :: k
real :: s

! Unallocated, then of another shape
a = tensors(3)
if (size(a) /= 3 .or. lbound(a, 1) /= 1) error stop 1
if (any(a(3)%values_ /= [3.0, 6.0])) error stop 2
a = tensors(2)
if (size(a) /= 2 .or. any(a(2)%values_ /= [2.0, 4.0])) error stop 3
! The same shape with other bounds: assigned, the bounds kept
deallocate(a)
allocate(a(0:1))
a = tensors(2)
if (lbound(a, 1) /= 0 .or. any(a(1)%values_ /= [2.0, 4.0])) error stop 4
! The same shape, with a pointer to an element: its storage is kept
allocate(b(2))
b(1)%values_ = [0.0]
b(2)%values_ = [0.0]
p => b(2)
b = tensors(2)
if (any(p%values_ /= [2.0, 4.0])) error stop 5
! The copy of an ASSOCIATE selector, in a loop
s = 0
do k = 1, 4
    associate(t => tensors(k))
        s = s + t(k)%values_(2)
    end associate
end do
if (abs(s - 20.0) > 1e-6) error stop 6
! Numbers: unallocated, another shape, the same shape with other bounds
x = numbers(4)
x = numbers(2)
if (size(x) /= 2 .or. lbound(x, 1) /= 1) error stop 7
deallocate(x)
allocate(x(0:2))
x = numbers(3)
if (lbound(x, 1) /= 0 .or. any(x /= [1.0, 2.0, 3.0])) error stop 8
print *, "ok"
end program
