program derived_types_66
   implicit none

   type :: val_type
      integer :: origin = 3
   end type

   type(val_type), allocatable :: val
   allocate(val)

   if (val%origin /= 3) error stop
end program derived_types_66
