module gpu_metal_282_m
   implicit none

   integer, parameter :: sel = 2

   type :: op_t
      ! A rank-2 component, with distinct extents, so a bound that stands
      ! for the whole component rather than one dimension is a wrong
      ! number and not merely a different spelling of the right one.
      real, allocatable :: w(:,:)
   end type op_t

end module gpu_metal_282_m
