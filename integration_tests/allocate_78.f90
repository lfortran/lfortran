! A negative extent in ALLOCATE gives a zero-size array.
program allocate_78
implicit none
integer, allocatable :: a(:), b(:,:)
integer :: n
n = -2
allocate(a(n))
print *, size(a)
if (size(a) /= 0) error stop 1
allocate(b(3,n))
if (size(b) /= 0 .or. size(b,1) /= 3 .or. size(b,2) /= 0) error stop 2
deallocate(a)
allocate(a(n+5))
if (size(a) /= 3) error stop 3
end program
