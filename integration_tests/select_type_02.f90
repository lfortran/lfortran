module derived_type_collection
implicit none

   type point
      real :: x, y
   end type point

end module derived_type_collection

program select_type_02
use derived_type_collection
implicit none

   type, extends(point) :: point_3d
      real :: z
   end type point_3d

   type, extends(point) :: color_point
      integer :: color
   end type color_point

   type(point), target :: p
   type(point_3d), target :: p3d
   type(color_point), target :: cp
   class(point), pointer :: p_or_cp

   p%x = 1.0
   p%y = 2.0

   p3d%x = 3.0
   p3d%y = 4.0
   p3d%z = 5.0

   cp%x = 6.0
   cp%y = 7.0
   cp%color = 8

   p_or_cp => cp

   select type ( an => p_or_cp )
      class is ( point )
         print *, "point: ", an%x, an%y
      type is ( color_point )
         print *, "color_point: ", an%x, an%y, an%color
   end select

   ! select type ( an => p_or_cp )
   !    class is ( point )
   !       print *, "point: ", an%x, an%y
   !    type is ( point_3d )
   !       print *, "point3d: ", an%x, an%y, an%z
   ! end select

end program select_type_02
