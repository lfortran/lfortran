module gpu_cuda_cpu_01_m
  implicit none
  type :: transform_t
  contains
    procedure :: apply => my_apply
  end type
  interface
    elemental module function my_apply(self, x) result(y)
      class(transform_t), intent(in) :: self
      real, intent(in) :: x
      real :: y
    end function
  end interface
end module

submodule (gpu_cuda_cpu_01_m) gpu_cuda_cpu_01_s
  implicit none
contains
  module procedure my_apply
    y = x * 2.0
  end procedure
end submodule
