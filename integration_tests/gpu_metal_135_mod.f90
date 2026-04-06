module gpu_metal_135_m
implicit none
type :: my_t
  integer :: val
end type
interface
  pure module function make_t(v) result(t)
    integer, intent(in) :: v
    type(my_t) :: t
  end function
end interface
end module

submodule(gpu_metal_135_m) gpu_metal_135_sub
implicit none
contains
module procedure make_t
  t%val = v
end procedure
end submodule
