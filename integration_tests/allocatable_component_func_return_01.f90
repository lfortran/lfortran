module allocatable_component_func_return_01_mod
   implicit none

   type :: my_layer_type
      integer, dimension(:), allocatable :: input_shape
   end type my_layer_type

contains

   function make_layer(n) result(layer)
      integer, intent(in) :: n
      type(my_layer_type) :: layer

      if (allocated(layer%input_shape)) then
         error stop "input_shape unexpectedly allocated on entry"
      end if

      allocate(layer%input_shape(n))
      layer%input_shape = n
   end function make_layer

end module allocatable_component_func_return_01_mod

program allocatable_component_func_return_01
   use allocatable_component_func_return_01_mod
   implicit none

   type(my_layer_type) :: layer

   layer = make_layer(1)
   if (size(layer%input_shape) /= 1) error stop "wrong size after 1st call"

   layer = make_layer(2)
   if (size(layer%input_shape) /= 2) error stop "wrong size after 2nd call"
end program allocatable_component_func_return_01
