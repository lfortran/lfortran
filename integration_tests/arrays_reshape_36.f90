module arrays_reshape_36_mod
  implicit none

  type :: multihead_attention_layer
     integer :: sequence_length
     integer :: model_dimension
  end type multihead_attention_layer

contains

  subroutine test_multihead_attention_combine_heads(attention, output_flat)
    type(multihead_attention_layer), intent(in) :: attention
    real, intent(out) :: output_flat(attention%sequence_length * attention%model_dimension)

    real :: output(attention%sequence_length, attention%model_dimension)
    integer :: i, j, k

    ! Fill output with predictable values
    k = 0
    do j = 1, attention%model_dimension
       do i = 1, attention%sequence_length
          k = k + 1
          output(i,j) = real(k)
       end do
    end do

    ! Flatten the array
    output_flat = reshape(output, shape(output_flat))

  end subroutine test_multihead_attention_combine_heads

end module arrays_reshape_36_mod


program arrays_reshape_36
  use arrays_reshape_36_mod
  implicit none

  type(multihead_attention_layer) :: attention
  real, allocatable :: result(:)
  integer :: i

  attention%sequence_length = 2
  attention%model_dimension = 3

  allocate(result(attention%sequence_length * attention%model_dimension))

  call test_multihead_attention_combine_heads(attention, result)

  ! Test the values
  do i = 1, size(result)
     if (result(i) /= real(i)) then
        error stop "incorrect reshape result"
     end if
  end do

  print *, "OK"

end program arrays_reshape_36