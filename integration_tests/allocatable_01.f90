! Allocatable arrays and strings in C/C++ backends: declarations lower
! to plain values/descriptors and allocated() works on arrays.
program allocatable_01
implicit none
integer, allocatable :: a(:)
character(len=:), allocatable :: s
print *, allocated(a)
if (allocated(a)) error stop 1
allocate(a(3))
a(1) = 1
a(2) = 2
a(3) = 3
if (.not. allocated(a)) error stop 2
if (a(2) /= 2) error stop 3
call fill(s)
print *, s
if (s /= "hi") error stop 4
contains
subroutine fill(t)
character(len=:), allocatable, intent(out) :: t
t = "hi"
end subroutine fill
end program allocatable_01
