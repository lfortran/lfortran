module nf_embedding_layer

   implicit none

   private
   public :: embedding_layer

   type :: embedding_layer
      integer :: sequence_length, vocab_size, model_dimension
      integer :: positional

      real, allocatable :: weights(:, :)
      real, allocatable :: output(:, :)
      real, allocatable :: dw(:, :) ! weight gradients

   contains

      procedure :: set_params

   end type embedding_layer

   interface
      module subroutine set_params(self, params)
         class(embedding_layer), intent(in out) :: self
         real, intent(in), target :: params(:)
      end subroutine set_params
   end interface
end module nf_embedding_layer

submodule(nf_embedding_layer) nf_embedding_layer_submodule
   implicit none
contains
   module subroutine set_params(self, params)
      class(embedding_layer), intent(in out) :: self
      real, intent(in), target :: params(:)

      real, pointer :: p_(:,:)

      associate(n => self % vocab_size * self % model_dimension)
         ! reshape the weights
         p_(1:self % vocab_size, 1:self % model_dimension) => params(1 : n)
         self % weights = p_
      end associate
   end subroutine set_params
end submodule nf_embedding_layer_submodule

program associate_30
   use nf_embedding_layer
   implicit none

   type(embedding_layer) :: layer
   real, target :: p(12)
   integer :: i

   layer%vocab_size = 2
   layer%model_dimension = 2

   do i = 1, 12
      p(i) = real(i)
   end do

   call layer%set_params(p)

   print *, layer%weights
   if (any(layer%weights /= reshape([1.0, 2.0, 3.0, 4.0], [2, 2]))) error stop
end program associate_30
