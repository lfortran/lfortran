program realloc_lhs_26
! Intrinsic assignment to an unallocated allocatable array component from
! a whole allocatable array component takes the bounds of the right-hand side
implicit none
type :: item_t
    integer :: v = 0
    integer, allocatable :: q(:)
end type
type :: data_t
    integer, allocatable :: a(:)
    real, allocatable :: b(:,:)
    character(len=:), allocatable :: c(:)
    type(item_t), allocatable :: e(:)
end type
type(data_t) :: source, dest, arr(2)

allocate(source%a(0:0), source=42)
dest%a = source%a
if (lbound(dest%a, 1) /= 0) error stop
if (ubound(dest%a, 1) /= 0) error stop
if (dest%a(0) /= 42) error stop

allocate(source%b(-1:1, 3:4))
source%b = 2.5
dest%b = source%b
if (any(lbound(dest%b) /= [-1, 3])) error stop
if (any(ubound(dest%b) /= [1, 4])) error stop
if (any(abs(dest%b - 2.5) > 1e-6)) error stop

allocate(arr(2)%a(5:7))
arr(2)%a = [1, 2, 3]
arr(1)%a = arr(2)%a
if (lbound(arr(1)%a, 1) /= 5) error stop
if (ubound(arr(1)%a, 1) /= 7) error stop
if (any(arr(1)%a /= [1, 2, 3])) error stop

allocate(character(len=3) :: source%c(4:5))
source%c = ['abc', 'def']
dest%c = source%c
if (lbound(dest%c, 1) /= 4) error stop
if (len(dest%c) /= 3) error stop
if (dest%c(5) /= 'def') error stop

allocate(source%e(0:1))
source%e(0)%v = 3
source%e(1)%v = 4
allocate(source%e(1)%q(-1:0))
source%e(1)%q = [9, 8]
dest%e = source%e
if (lbound(dest%e, 1) /= 0) error stop
if (ubound(dest%e, 1) /= 1) error stop
if (dest%e(0)%v /= 3) error stop
if (dest%e(1)%v /= 4) error stop
if (allocated(dest%e(0)%q)) error stop
if (lbound(dest%e(1)%q, 1) /= -1) error stop
if (any(dest%e(1)%q /= [9, 8])) error stop

! Same shape: no reallocation, the left-hand side keeps its bounds
dest%a = [7]
if (lbound(dest%a, 1) /= 0) error stop
if (dest%a(0) /= 7) error stop

! An array section of a component has lower bound 1
arr(1)%a = source%a(0:0)
if (lbound(arr(1)%a, 1) /= 1) error stop
if (arr(1)%a(1) /= 42) error stop

deallocate(dest%a)
dest%a = arr(2)%a(:)
if (lbound(dest%a, 1) /= 1) error stop
if (any(dest%a /= [1, 2, 3])) error stop

print *, lbound(dest%a, 1), lbound(dest%b), lbound(dest%c, 1)
end program
