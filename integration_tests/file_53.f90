program format_53
   implicit none

   integer :: ia_1d(20), ia_2d(10,2)

   integer :: i, j

   ia_1d(:) = [(i, i=1, 20)]
   do, j=1, 2
      do, i=1, 10
         ia_2d(i,j) = j*100 + i
      end do
   end do

   WRITE (*, 77751 ) 1, 2, 3, 9999,  &
      (ia_1d(i), i = 1, 20)
   print *, '-----------'

! Should repeat the 3(1I3), but instead is getting lost
! and doing some sort of i3,i4,3(i3) repetition.

   WRITE (*, 77752 ) 1, 2, 3, 9999,  &
      ((ia_2d(i,j), j = 1, 2), i = 1, 10)

77751 FORMAT ( 3(1I3), I4, 10(1I3) )
77752 FORMAT ( 3(1I3), I4, 3(1I3) )

end program format_53
