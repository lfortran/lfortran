module nf_linear2d_layer
   implicit none
   private
   public :: linear2d_layer

   type :: linear2d_layer
      real, allocatable :: weights(:,:)
      real, allocatable :: biases(:)
   contains
      procedure :: get_params_ptr
   end type

contains

   subroutine get_params_ptr(self, w_ptr, b_ptr)
      class(linear2d_layer), intent(in), target :: self
      real, pointer, intent(out) :: w_ptr(:), b_ptr(:)

      w_ptr(1:size(self%weights)) => self%weights
      b_ptr => self%biases
   end subroutine

end module


program associate_27
   use nf_linear2d_layer
   implicit none

   type(linear2d_layer) :: layer
   real, pointer :: w(:), b(:)

   allocate(layer%weights(2,2))
   allocate(layer%biases(2))

   layer%weights = reshape([1.,2.,3.,4.], [2,2])
   layer%biases  = [5.,6.]

   call layer%get_params_ptr(w, b)

   if (.not. associated(w) .or. .not. associated(b)) error stop

   w(1) = -1.
   b(2) = -2.

   if (layer%weights(1,1) /= -1.) error stop
   if (layer%biases(2)    /= -2.) error stop

end program
