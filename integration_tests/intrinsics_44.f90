program intrinsics_44
   implicit none
   integer, dimension(5):: a = [1, 2, 3, 4, 5]
   integer, dimension(3,3) :: b = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])

   print '(5i3)', a
   a = cshift(a, shift = 2)
   print '(5i3)', a
   a = cshift(a, shift = 1)
   print '(5i3)', a
   if(any(a /= [ 4, 5, 1, 2, 3 ])) error stop

   print *
   b = cshift(b, shift = 2, dim = 2)
   print '(3i3)', b(1,:)
   print '(3i3)', b(2,:)
   print '(3i3)', b(3,:)
   if(any(b /= reshape( [ 7, 8, 9, 1, 2, 3, 4, 5, 6 ], [ 3, 3 ]))) error stop

end program
