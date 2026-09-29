! Intrinsic assignment to an allocatable component reallocates it to the
! shape of the right hand side (F2018 10.2.1.3), with --realloc-lhs-arrays.
program derived_types_183
implicit none

type :: point
    integer :: x
end type point

type :: container
    real, allocatable :: v(:)
    integer, allocatable :: m(:,:)
    character(len=3), allocatable :: c(:)
    type(point), allocatable :: p(:)
end type container

type(container) :: t

! grow an already allocated component
allocate(t%v(2))
t%v = [9.0, 9.0]
t%v = [1.0, 2.0, 3.0]
if (size(t%v) /= 3) error stop "v was not grown"
if (abs(t%v(1) - 1.0) > 1.0e-6) error stop "v(1) is wrong after growing"
if (abs(t%v(3) - 3.0) > 1.0e-6) error stop "v(3) is wrong after growing"

! shrink it again
t%v = [7.0]
if (size(t%v) /= 1) error stop "v was not shrunk"
if (abs(t%v(1) - 7.0) > 1.0e-6) error stop "v(1) is wrong after shrinking"

! a component that starts unallocated is allocated by the assignment
if (allocated(t%m)) error stop "m should start unallocated"
t%m = reshape([1, 2, 3, 4, 5, 6], [2, 3])
if (.not. allocated(t%m)) error stop "m was not allocated"
if (size(t%m, 1) /= 2) error stop "m has a wrong extent 1"
if (size(t%m, 2) /= 3) error stop "m has a wrong extent 2"
if (t%m(2, 3) /= 6) error stop "m(2,3) is wrong"

! a rank 2 component is reshaped by the assignment
t%m = reshape([1, 2, 3, 4, 5, 6, 7, 8], [4, 2])
if (size(t%m, 1) /= 4) error stop "m has a wrong extent 1 after reshaping"
if (size(t%m, 2) /= 2) error stop "m has a wrong extent 2 after reshaping"
if (t%m(4, 2) /= 8) error stop "m(4,2) is wrong after reshaping"

! a character component
t%c = ["abc", "def"]
if (size(t%c) /= 2) error stop "c has a wrong size"
t%c = ["ghi"]
if (size(t%c) /= 1) error stop "c was not shrunk"
if (t%c(1) /= "ghi") error stop "c(1) is wrong"

! a derived type component
t%p = [point(1), point(2)]
if (size(t%p) /= 2) error stop "p has a wrong size"
t%p = [point(3), point(4), point(5)]
if (size(t%p) /= 3) error stop "p was not grown"
if (t%p(3)%x /= 5) error stop "p(3)%x is wrong"

print *, "ok"
end program derived_types_183
