module functions_56_mod
  implicit none
  private
  public :: mha_layer

  type :: mha_layer
    integer :: sequence_length, head_size, n_heads
    real, allocatable :: v_heads(:,:,:)
  contains
    procedure :: split_heads
    procedure :: do_backward
  end type mha_layer

  interface
    pure module function split_heads(self, input) result(output)
      class(mha_layer), intent(in) :: self
      real, intent(in) :: input(:)
      real :: output(self%sequence_length, self%head_size, self%n_heads)
    end function split_heads

    pure module subroutine do_backward(self)
      class(mha_layer), intent(inout) :: self
    end subroutine do_backward
  end interface

end module functions_56_mod


submodule(functions_56_mod) functions_56_smod
  implicit none
contains

  pure module function split_heads(self, input) result(output)
    class(mha_layer), intent(in) :: self
    real, intent(in) :: input(:)
    real :: output(self%sequence_length, self%head_size, self%n_heads)

    output = 1.0
  end function split_heads


  pure module subroutine do_backward(self)
    class(mha_layer), intent(inout) :: self
    real :: temp(2)

    temp = 1.0
    self%v_heads = self%split_heads(temp)

  end subroutine do_backward

end submodule functions_56_smod