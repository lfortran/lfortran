program intrinsics_351
integer, allocatable :: i(:)
integer :: ii(4,4)
allocate(i(4))
i = [1,2,3,4]
ii = spread(i, dim = 2, ncopies=4)
print *, sum(ii)
if ( sum(ii) /= 40 ) error stop
end program intrinsics_351
