module gpu_metal_370_mod
implicit none
integer :: starts = 0, ends = 0
contains
integer function first()
    starts = starts + 1
    first = 1
end function

integer function last(n)
    integer, intent(in) :: n
    ends = ends + 1
    last = n
end function
end module

program gpu_metal_370
! The limits of a `do concurrent` with two indices are each evaluated once,
! before the loop runs, also when the host evaluates them for the launch and
! to allocate the components the loop writes.
use gpu_metal_370_mod
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(2, 3)
integer :: i, j
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
do concurrent (i = 1:last(2), j = first():last(3))
    t(i, j) = f(c(i:i + j - 1))
end do
print *, starts, ends, size(t(1, 1)%v), size(t(2, 3)%v)
if (starts /= 1 .or. ends /= 2) error stop 1
if (size(t(1, 1)%v) /= 1 .or. size(t(2, 3)%v) /= 3) error stop 2
if (any(t(2, 3)%v /= c(2:4))) error stop 3
print *, "ok"
contains
pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function
end program
