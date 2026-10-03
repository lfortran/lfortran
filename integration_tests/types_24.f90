module nf_flatten_layer
   implicit none
   private
   public :: flatten_layer

   type :: flatten_layer
      real, allocatable :: gradient_2d(:,:)
   contains
      procedure :: backward
   end type flatten_layer

contains

   subroutine backward(self, input, gradient)
      class(flatten_layer), intent(in out) :: self
      real, intent(in) :: input(..)
      real, intent(in) :: gradient(:)

      select rank(input)
       rank(2)
         self%gradient_2d = reshape(gradient, shape(input))
       rank default
         error stop "Unsupported rank of input"
      end select

   end subroutine backward

end module nf_flatten_layer


program main
   use nf_flatten_layer
   implicit none

   type(flatten_layer) :: layer
   real :: input(2,3)
   real :: gradient(6)

   integer :: i

   ! Initialize input
   input = reshape([1.,2.,3.,4.,5.,6.], shape(input))

   ! Suppose this is gradient from next layer (flattened)
   gradient = [10.,20.,30.,40.,50.,60.]

   call layer%backward(input, gradient)

   print *, "Input:"
   print *, input
   if (any(input /= reshape([1.,2.,3.,4.,5.,6.], [2, 3]))) error stop

   print *, "Gradient reshaped back to 2D:"
   print *, layer%gradient_2d
   if (any(layer%gradient_2d /= reshape([10.,20.,30.,40.,50.,60.], [2, 3]))) error stop

end program main
