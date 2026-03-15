program dealloc_06
  implicit none

  type :: linear2d_layer
    real, allocatable :: weights(:,:)
  end type linear2d_layer

  type :: attention_layer
    integer :: model_dimension
    type(linear2d_layer) :: query_layer
  end type attention_layer

  type(attention_layer) :: self
  real :: params(80)

  self % model_dimension = 4
  allocate(self % query_layer % weights(4, 4))
  self % query_layer % weights = 0.1

  params = 0.5

  call set_params(self, params)

  if (any(self % query_layer % weights /= 0.5)) then
     error stop "weights value mismatch"
  end if

contains

  subroutine set_params(self, params)
    type(attention_layer), intent(inout) :: self
    real, intent(in) :: params(:)
    self % query_layer % weights = reshape(params, [4,4])
  end subroutine set_params

end program dealloc_06
