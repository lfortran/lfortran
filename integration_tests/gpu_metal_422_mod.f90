! The module and submodule of gpu_metal_422, compiled separately from the
! program: the check reads the body of the module procedure from the
! submodule's module file.
module gpu_metal_422_m
  implicit none
  interface
    pure module function f(n) result(r)
      integer, intent(in) :: n
      integer :: r
    end function
  end interface
end module

submodule (gpu_metal_422_m) gpu_metal_422_s
  implicit none
contains
  module procedure f
    real(8) :: t
    t = n
    r = int(t)
  end procedure
end submodule
