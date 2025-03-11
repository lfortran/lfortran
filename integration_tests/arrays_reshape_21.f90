program arrays_reshape_21
   implicit none
   integer, parameter :: arr1(*, *) = reshape( [1, 2, 3, 4], [2, 2] )
   integer, parameter :: arr2(*) = reshape(arr1, [4])
   print *, arr1
   print *, arr2
   if( any(arr2 /= [1, 2, 3, 4]) ) error stop
end program arrays_reshape_21
