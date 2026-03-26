program derived_types_131
implicit none

type :: point
    integer :: x
    integer :: y
end type

! reshape of derived type array as compile-time parameter
type(point), parameter :: v(*) = reshape([point(1, 2)], [1])
type(point), parameter :: grid(2,2) = reshape( &
    [point(1,1), point(2,2), point(3,3), point(4,4)], [2,2])

if (v(1)%x /= 1) error stop
if (v(1)%y /= 2) error stop

if (grid(1,1)%x /= 1) error stop
if (grid(2,1)%x /= 2) error stop
if (grid(1,2)%x /= 3) error stop
if (grid(2,2)%x /= 4) error stop
if (grid(2,2)%y /= 4) error stop

print *, "ok"
end program
