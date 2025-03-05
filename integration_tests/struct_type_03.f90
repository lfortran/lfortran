program struct_type_03

   type :: struct
      real :: field = 5.0
   end type struct

   type(struct) :: array(3)
   write(*,*) sum(array(:)%field)

   if (sum(array(:)%field) /= 15.0) error stop 

end program