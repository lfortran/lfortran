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
   p_or_cp => cp
   select type ( an => p_or_cp )
   class is ( point )
                     ! "class ( point ) :: an" is implied here
      print *, an%x, an%y     ! this block gets executed
   type is ( point_3d )
                     ! "type ( point_3d ) :: an" is implied here
      print *, an%x, an%y, an%z
   end select

end program select_type_02
