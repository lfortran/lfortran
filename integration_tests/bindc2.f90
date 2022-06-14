program bindc2
use iso_c_binding, only: c_loc, c_ptr, c_f_pointer
type(c_ptr) :: queries
integer :: idx = 1
integer(2), target :: xv(3, 4)
integer :: newshape(2)
integer(2), pointer :: x(:, :)

newshape(1) = 2
newshape(2) = 3

x => xv

if( lbound(xv, 1) /= 1) error stop
if( ubound(xv, 1) /= 3) error stop
if( lbound(xv, 2) /= 1) error stop
if( ubound(xv, 2) /= 4) error stop
if( size(xv) /= 12 ) error stop

call c_f_pointer(queries, x, newshape)

if( lbound(xv, 1) /= 1) error stop
if( ubound(xv, 1) /= 3) error stop
if( lbound(xv, 2) /= 1) error stop
if( ubound(xv, 2) /= 4) error stop
if( size(xv) /= 12 ) error stop

print *, lbound(x, 1), ubound(x, 1)
print *, lbound(x, 2), ubound(x, 2)
print *, size(x)
if( lbound(x, 1) /= 1) error stop
if( ubound(x, 1) /= 2) error stop
if( lbound(x, 2) /= 1) error stop
if( ubound(x, 2) /= 3) error stop
if( size(x) /= 6 ) error stop

end program
