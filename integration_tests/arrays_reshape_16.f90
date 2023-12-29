program arrays_reshape_16
implicit none

integer, allocatable :: array_const_1(:, :)
integer :: i23_shape(2) = [4, 2]
integer :: i23_shape_e(2)
allocate(array_const_1(2, 4))
array_const_1 = reshape([-14, 3, 0, -2, 19, 1, 20, 21], [2, 4])

print *, array_const_1(1, :)
print *, array_const_1(2, :)
array_const_1 = reshape(array_const_1, i23_shape)

print *, shape(array_const_1)
print *, lbound(array_const_1, 1), ubound(array_const_1, 1)
print *, lbound(array_const_1, 2), ubound(array_const_1, 2)
print *, size(array_const_1, 1), size(array_const_1, 2)
if( lbound(array_const_1, 1) /= 1 ) error stop
if( ubound(array_const_1, 1) /= 4 ) error stop
if( lbound(array_const_1, 2) /= 1 ) error stop
if( ubound(array_const_1, 2) /= 2 ) error stop
if( size(array_const_1, 1) /= 4 ) error stop
if( size(array_const_1, 2) /= 2 ) error stop

i23_shape_e = shape(array_const_1)
if( i23_shape_e(1) /= 4 ) error stop
if( i23_shape_e(2) /= 2 ) error stop

print *, array_const_1(1, :)
print *, array_const_1(2, :)
print *, array_const_1(3, :)
print *, array_const_1(4, :)
if( array_const_1(1, 1) /= -14 ) error stop
if( array_const_1(1, 2) /= 19 ) error stop
if( array_const_1(2, 1) /= 3 ) error stop
if( array_const_1(2, 2) /= 1 ) error stop
if( array_const_1(3, 1) /= 0 ) error stop
if( array_const_1(3, 2) /= 20 ) error stop
if( array_const_1(4, 1) /= -2 ) error stop
if( array_const_1(4, 2) /= 21 ) error stop
end program
