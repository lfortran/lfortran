module submodule_54_mod
  implicit none

  type :: array_type
    integer :: id = -1
    procedure(get_partial_val), pass(this), pointer :: ptr1 => null()
    procedure(get_partial_val), pass(this), pointer :: ptr2 => null()
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
end module submodule_54_mod

submodule(submodule_54_mod) submodule_54_impl
  implicit none
contains
  pure module subroutine get_partial_add_val(this, upstream_grad, output)
    class(array_type), intent(in) :: this
    real, dimension(:,:), intent(in) :: upstream_grad
    real, dimension(:,:), intent(out) :: output
    output = upstream_grad
  end subroutine get_partial_add_val

  pure subroutine get_partial_with_reshape(this, upstream_grad, output)
    class(array_type), intent(in) :: this
    real, dimension(:,:), intent(in) :: upstream_grad
    real, dimension(:,:), intent(out) :: output
    real, dimension(size(upstream_grad,1), size(upstream_grad,2)) :: tmp
    tmp = reshape(upstream_grad, [size(upstream_grad,1), size(upstream_grad,2)])
    output = tmp
  end subroutine get_partial_with_reshape

  module subroutine setup(c)
    type(array_type), intent(inout) :: c
    c%ptr1 => get_partial_add_val
    c%ptr2 => get_partial_with_reshape
  end subroutine setup
end submodule submodule_54_impl

program submodule_54
  use submodule_54_mod
  implicit none
  type(array_type) :: c
  real, dimension(2,3) :: input, output

  input = 1.0
  output = 0.0
  call setup(c)
  call c%ptr1(input, output)
  if (any(output /= input)) error stop "ptr1 mismatch"
  output = 0.0
  call c%ptr2(input, output)
  if (any(output /= input)) error stop "ptr2 mismatch"
  print *, "PASSED"
end program submodule_54
