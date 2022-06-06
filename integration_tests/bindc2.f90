program bindc2
use iso_c_binding, only: c_loc, c_ptr, c_f_pointer
type(c_ptr) :: queries
integer :: idx = 1
integer(2), target :: xv(3, 4)
integer :: newshape(2)
newshape(1) = 2
newshape(2) = 3
integer(2), pointer :: x(:, :)
x => xv

if( lbound(xv, 1) /= 1) error stop
if( ubound(xv, 1) /= 3) error stop
if( lbound(xv, 2) /= 1) error stop
if( ubound(xv, 2) /= 4) error stop
if( size(xv) /= 12 ) error stop

call c_f_pointer(queries, x, newshape)

if( lbound(xv, 1) /= 1) error stop
if( ubound(xv, 1) /= 2) error stop
if( lbound(xv, 2) /= 1) error stop
if( ubound(xv, 2) /= 3) error stop
if( size(xv) /= 6 ) error stop

! print *, c_loc(x(idx))
end program
