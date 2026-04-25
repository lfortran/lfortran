program arrays_reshape_37
! Test reshape of character array assigned to deferred-length
! allocatable character array (realloc-lhs).
implicit none
character(:), allocatable :: a(:,:)
character(:), allocatable :: b(:)

! 2D reshape of character array
a = reshape(['ab','cd'], shape=[1,2])
if (a(1,1) /= 'ab') error stop
if (a(1,2) /= 'cd') error stop
if (size(a,1) /= 1) error stop
if (size(a,2) /= 2) error stop
if (len(a) /= 2) error stop

! Reassign with different shape
a = reshape(['xx','yy','zz','ww'], shape=[2,2])
if (a(1,1) /= 'xx') error stop
if (a(2,1) /= 'yy') error stop
if (a(1,2) /= 'zz') error stop
if (a(2,2) /= 'ww') error stop
if (size(a,1) /= 2) error stop
if (size(a,2) /= 2) error stop

! 1D character array assignment
b = ['hello','world']
if (b(1) /= 'hello') error stop
if (b(2) /= 'world') error stop
if (len(b) /= 5) error stop

! Verify any() with reshape of character array
if (any(a /= reshape(['xx','yy','zz','ww'], shape=[2,2]))) error stop

print *, "PASS"
end program
