module arrays_115_mod
  implicit none

  type :: multihead_attention_layer
     integer :: sequence_length
     integer :: model_dimension
  end type multihead_attention_layer

contains

  function combine_heads(self, input) result(output)
    class(multihead_attention_layer), intent(in) :: self
    real, intent(in), target :: input(:, :, :)
    real :: output(self%sequence_length, self%model_dimension)
    integer :: seq, i, j

    ! Pointer used to mimic temporary flattening
    do concurrent (seq = 1:self%sequence_length)
       output(seq, :) = reshape(input(:, :, 1), [self%model_dimension])
    end do

  end function combine_heads

end module arrays_115_mod


program arrays_115
  use arrays_115_mod
  implicit none

  type(multihead_attention_layer) :: self
  real :: input(2,2,1)
  real, allocatable :: output(:,:)
  integer :: i,j

  self%sequence_length = 3
  self%model_dimension = 4

  ! initialize input
  input(:,:,1) = reshape([1.0,2.0,3.0,4.0],[2,2])

  output = combine_heads(self, input)
  if (size(output,1) /= self%sequence_length .or. size(output,2) /= self%model_dimension) then
     error stop "output size mismatch"
  end if

  ! Check output values
  do i = 1, self%sequence_length
     if (any(output(i, :) /= [1.0, 2.0, 3.0, 4.0])) then
        error stop "output values mismatch"
     end if
  end do

  print *, "Passed"

end program arrays_115