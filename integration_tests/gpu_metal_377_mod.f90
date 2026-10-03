module gpu_metal_377_helpers
  implicit none
  integer :: calls = 5
contains
  pure integer function add_offset(x)
    integer, intent(in) :: x
    add_offset = x + 10
  end function
end module

module gpu_metal_377_m
  implicit none
  interface
    pure module function compute(x) result(y)
      integer, intent(in) :: x
      integer :: y
    end function
  end interface
end module

submodule(gpu_metal_377_m) gpu_metal_377_impl
  use gpu_metal_377_helpers, only : add_offset
  implicit none
contains
  pure module function compute(x) result(y)
    integer, intent(in) :: x
    integer :: y
    y = add_offset(x)
  end function
end submodule
