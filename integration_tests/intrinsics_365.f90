program intrinsics_365
implicit none
real, allocatable :: array(:, :)
allocate(array(2:5, 5:10))
print *, is_contiguous(array)
if ( .not. is_contiguous(array) ) error stop
end program
