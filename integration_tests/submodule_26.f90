module submodule_26_mod
  implicit none
  private
  public :: grid

  interface
    module function grid(x) result(y)
      real(8), intent(in) :: x(:)
      real(8) :: y(size(x))
    end function
  end interface

contains

  pure function helper(x) result(y)
    real(8), intent(in) :: x(:)
    real(8) :: y(size(x))
    y = x * 2d0
  end function

end module submodule_26_mod

submodule(submodule_26_mod) submodule_26_sub
  implicit none
contains

  module procedure grid
    y = helper(x)
  end procedure

end submodule submodule_26_sub

program submodule_26
  use submodule_26_mod, only: grid
  implicit none
  real(8) :: x(3)
  real(8) :: y(3)

  x = [1d0, 2d0, 3d0]
  y = grid(x)
  if (any(y /= [2d0, 4d0, 6d0])) error stop
  print *, "ok"
end program submodule_26
