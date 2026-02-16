module nf_multihead_attention_layer
  implicit none
  type :: multihead_attention_layer
    integer :: seq_len, model_dim, n_heads, head_size
    real, allocatable :: output_layer(:, :)
    real, allocatable :: d_output(:, :, :)
  contains
    procedure :: sdpa_backward
    procedure :: split_heads
  end type
contains

  subroutine sdpa_backward(self)
    class(multihead_attention_layer), intent(inout) :: self

    if (.not. allocated(self%output_layer)) error stop "Not allocated"

    self%d_output = self%split_heads(self%output_layer)
  end subroutine

  pure function split_heads(self, input) result(out)
    class(multihead_attention_layer), intent(in) :: self
    real, intent(in) :: input(:, :)
    real :: out(self%seq_len, self%head_size, self%n_heads)
    integer :: h

    if (size(input,2) /= self%model_dim) error stop "Wrong shape"

    do h = 1, self%n_heads
      out(:,:,h) = input(:, (h-1)*self%head_size+1:h*self%head_size)
    end do
  end function

end module

program test
  use nf_multihead_attention_layer
  implicit none

  type(multihead_attention_layer) :: L

  L%seq_len = 2
  L%model_dim = 4
  L%n_heads = 2
  L%head_size = 2

  allocate(L%output_layer(2,4))
  L%output_layer = reshape([1.,2.,3.,4.,5.,6.,7.,8.], [2,4])

  call L%sdpa_backward()

  print *, L%d_output
  if (any(L%d_output /= reshape([1.,2.,3.,4.,5.,6.,7.,8.], [2, 2, 2]))) error stop
end program
