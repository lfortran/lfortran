program derived_type_shape_pointer_02
   implicit none
   type :: inner_t
      integer :: rank
   end type inner_t

   type :: outer_t
      type(inner_t), pointer :: arr(:,:)
   end type outer_t

   type(inner_t), target :: data(3, 4)
   type(outer_t) :: x
   integer :: s(2)

   x%arr => data
   s = shape(x%arr)
   if (s(1) /= 3) error stop
   if (s(2) /= 4) error stop
   print *, s
end program derived_type_shape_pointer_02
