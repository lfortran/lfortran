module gpu_metal_385_m
  implicit none
  interface
    elemental module function scaled(k, x) result(y)
      integer, intent(in) :: k
      real, intent(in) :: x
      real :: y
    end function
  end interface
end module

submodule(gpu_metal_385_m) gpu_metal_385_s
  implicit none
contains
  module procedure scaled
    select case (k)
    case (1)
      y = 2*x
    case default
      error stop "scaled: unknown k"
    end select
  end procedure
end submodule
