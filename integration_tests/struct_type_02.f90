program struct_type_02

   type :: struct
      real :: field = 5.0
   end type struct

   type(struct) :: array(3)
   print*, array(:)%field

end program