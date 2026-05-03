module submodule_53_mod
  implicit none

  type :: array_type
    integer :: id = -1
    procedure(get_partial_val), pass(this), pointer :: get_partial_left_val => null()
  end type array_type

  interface
    pure module subroutine get_partial_val(this, upstream_grad, output)
      class(array_type), intent(in) :: this
      real, dimension(:,:), intent(in) :: upstream_grad
      real, dimension(:,:), intent(out) :: output
    end subroutine get_partial_val

    pure module subroutine get_partial_add_val(this, upstream_grad, output)
      class(array_type), intent(in) :: this
      real, dimension(:,:), intent(in) :: upstream_grad
      real, dimension(:,:), intent(out) :: output
    end subroutine get_partial_add_val

    module subroutine setup(c)
      type(array_type), intent(inout) :: c
    end subroutine setup
  end interface

end module submodule_53_mod

submodule(submodule_53_mod) submodule_53_impl
  implicit none

contains

  pure module subroutine get_partial_add_val(this, upstream_grad, output)
    class(array_type), intent(in) :: this
    real, dimension(:,:), intent(in) :: upstream_grad
    real, dimension(:,:), intent(out) :: output
    output = upstream_grad
  end subroutine get_partial_add_val

  module subroutine setup(c)
    type(array_type), intent(inout) :: c
    c%get_partial_left_val => get_partial_add_val
  end subroutine setup

end submodule submodule_53_impl
