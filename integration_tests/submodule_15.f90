module submodule_15_m
  implicit none

  interface
    module function value_from_submodule() result(v)
      integer :: v
    end function value_from_submodule
  end interface
end module submodule_15_m

submodule(submodule_15_m) submodule_15_s
  implicit none
contains
  module procedure value_from_submodule
    v = f()
  contains
    integer function f()
      f = 1
    end function f
  end procedure value_from_submodule
end submodule submodule_15_s

program submodule_15
  use submodule_15_m, only: value_from_submodule
  implicit none

  if (value_from_submodule() /= 1) error stop 1
end program submodule_15
