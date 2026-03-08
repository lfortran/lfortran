program implied_do_loops22
! Test: nested implied do loops in array constructors with derived types
implicit none

type :: point_t
    real :: x, y
end type

type(point_t) :: pts(1)
type(point_t) :: grid(4)
integer :: i, j

! Nested implied do loop with derived type constructor
pts = [( [(point_t(real(i), real(j)), j=1,1)], i=1,1)]
if (abs(pts(1)%x - 1.0) > 1e-6) error stop
if (abs(pts(1)%y - 1.0) > 1e-6) error stop

! Larger nested implied do loop
grid = [( [(point_t(real(i), real(j)), j=1,2)], i=1,2)]
if (abs(grid(1)%x - 1.0) > 1e-6) error stop
if (abs(grid(1)%y - 1.0) > 1e-6) error stop
if (abs(grid(2)%x - 1.0) > 1e-6) error stop
if (abs(grid(2)%y - 2.0) > 1e-6) error stop
if (abs(grid(3)%x - 2.0) > 1e-6) error stop
if (abs(grid(3)%y - 1.0) > 1e-6) error stop
if (abs(grid(4)%x - 2.0) > 1e-6) error stop
if (abs(grid(4)%y - 2.0) > 1e-6) error stop

print *, "OK"
end program
