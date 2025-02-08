program intrinsics_351
real :: xx(3,20)
real :: x(20) 
integer :: j, n
integer, allocatable :: i(:)
integer :: ii(4,4)
allocate(i(4))
i = [1,2,3,4]
ii = spread(i, dim = 2, ncopies=4)
print *, sum(ii)
if ( sum(ii) /= 40 ) error stop

do j = 1, 20
   xx(:,j) = j 
   x(j) = j 
end do
n = 3
xx = xx / spread(x, dim = 1, ncopies = n) 
print *, xx
if (any(xx /= 1)) error stop
end program intrinsics_351
