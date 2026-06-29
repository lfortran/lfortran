module class_array_deepcopy_01_mod
   implicit none

   type :: array_type
      integer, dimension(:), allocatable :: shape
      real, dimension(:), allocatable :: val
   end type array_type

   type, extends(array_type) :: batchnorm_array_type
      real, dimension(:), allocatable :: mean
   end type batchnorm_array_type

   type :: base_layer_type
      class(array_type), dimension(:,:), allocatable :: output
   end type base_layer_type

   type, extends(base_layer_type) :: batchnorm1d_layer_type
   end type batchnorm1d_layer_type

end module class_array_deepcopy_01_mod

program class_array_deepcopy_01
   use class_array_deepcopy_01_mod
   implicit none

   type(batchnorm1d_layer_type) :: layer
   class(base_layer_type), allocatable :: bn_layer

   allocate(batchnorm_array_type :: layer%output(1,1))
   
   bn_layer = layer

   if (.not. allocated(bn_layer)) error stop "bn_layer not allocated"
   select type (bn_layer)
   type is (batchnorm1d_layer_type)
      if (.not. allocated(bn_layer%output)) error stop "output not allocated"
   class default
      error stop "wrong dynamic type for bn_layer"
   end select

   print *, "ok"
end program class_array_deepcopy_01
