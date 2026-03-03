module submodule_35_util_mod
  implicit none
  type :: tensor_t
    integer :: rows, cols
  end type tensor_t
end module submodule_35_util_mod

module submodule_35_mod
  use submodule_35_util_mod, only: tensor_t
  implicit none
  interface
    pure module function reshape_tensor(t, flat) result(output)
      type(tensor_t), intent(in) :: t
      real, intent(in) :: flat(:)
      real :: output(t%rows, t%cols)
    end function reshape_tensor

    module subroutine fill(t)
      type(tensor_t), intent(inout) :: t
    end subroutine fill
  end interface
end module submodule_35_mod
