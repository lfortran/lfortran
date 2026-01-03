program arrays_op_18
implicit none

real, allocatable :: a(:)
real :: b(10, 7)
integer :: i, j
allocate(a(40))

a = 5.0
b = 6.0

a = [b, a]

print *, size(a), lbound(a, 1), ubound(a, 1)
if( size(a) /= 110 ) error stop

print *, a
do i = 1, 70
    if( a(i) /= 6.0 ) error stop
end do
do i = 71, 110
    if( a(i) /= 5.0 ) error stop
end do

end program
