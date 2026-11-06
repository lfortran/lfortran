program associate_66
! An ASSOCIATE on a host variable whose type `t` is hidden in the internal
! procedure by a local variable named `t`.
implicit none
type :: t
    integer :: v = 4
end type
type(t) :: x
call s([1.0, 2.0])
if (x%v /= 6) error stop 2
print *, "ok", x%v
contains
subroutine s(f)
real, intent(in) :: f(:)
integer :: t
t = 1
block
associate (a => x)
    if (a%v /= 4) error stop 1
    a%v = a%v + size(f)
end associate
end block
if (t /= 1) error stop 3
end subroutine
end program
