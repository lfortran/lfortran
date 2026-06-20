program derived_type_shape_pointer_03
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
   call shape_clone(x%arr, s)
   if (s(1) /= 3) error stop
   if (s(2) /= 4) error stop
   print *, s

contains

   subroutine shape_clone(y, out)
      type(inner_t), pointer :: y(:,:)
      integer, intent(out) :: out(2)
      out = shape(y)
   end subroutine shape_clone

end program derived_type_shape_pointer_03
