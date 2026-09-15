module gpu_metal_369_mod
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

program gpu_metal_369
! The limits of a `do concurrent` are evaluated once, before the loop runs,
! also when the host evaluates them for the launch and to allocate the
! components the loop writes.
use gpu_metal_369_mod
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6), x(4)
type(tt) :: t(2)
integer :: i
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
x = 0.0

do concurrent (i = first():last(4))
    x(i) = real(i)
end do
print *, starts, ends, x
if (starts /= 1 .or. ends /= 1) error stop 1
if (any(x /= [1.0, 2.0, 3.0, 4.0])) error stop 2

do concurrent (i = first():last(2))
    t(i) = f(c(1:i + 1))
end do
print *, starts, ends, size(t(1)%v), size(t(2)%v)
if (starts /= 2 .or. ends /= 2) error stop 3
if (size(t(1)%v) /= 2 .or. size(t(2)%v) /= 3) error stop 4
if (any(t(2)%v /= c(1:3))) error stop 5
print *, "ok"
contains
pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function
end program
