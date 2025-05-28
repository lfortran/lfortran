program arrays_90
   integer, parameter :: offset(*) = [0, 1, 2, 3, 4]
   integer, parameter :: offset2(2,3) = reshape([1, 2, 3, 2, 1, 3], [2, 3])
   integer :: temp(2)
   temp = offset(2:3) 
   if (any(temp /= [1, 2])) error stop
   temp = offset2(:, 2)
   print *, temp
   if (any(temp /= [3, 2])) error stop
end program