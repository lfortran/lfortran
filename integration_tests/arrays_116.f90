module arrays_116_mod
  implicit none

  type :: layer_t
     real, allocatable :: weights(:,:)
  end type layer_t

  type :: model_t
     type(layer_t) :: query_layer
  end type model_t

contains

  subroutine build_params(self)
    type(model_t), intent(in) :: self
    real, allocatable :: params(:)
    integer :: i
    real :: expected(4)

    ! Flatten weights
    allocate(params(size(self%query_layer%weights)))
    params = reshape(self%query_layer%weights, [size(self%query_layer%weights)])

    ! Expected values (column-major flattening)
    expected = [1.0,2.0,3.0,4.0]

    ! Check size
    if (size(params) /= size(expected)) then
       error stop "params size mismatch"
    end if

    ! Check values
    do i = 1, size(params)
       if (params(i) /= expected(i)) then
          error stop "params value mismatch"
       end if
    end do

  end subroutine build_params

end module arrays_116_mod


program arrays_116
  use arrays_116_mod
  implicit none

  type(model_t) :: model

  allocate(model%query_layer%weights(2,2))
  model%query_layer%weights = reshape([1.0,2.0,3.0,4.0],[2,2])

  call build_params(model)
end program arrays_116