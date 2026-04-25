module submodule_35_mod
  interface
    module function cube(x)
      real :: cube
      real, intent(in) :: x
    end function cube
  end interface
end module submodule_35_mod

submodule (submodule_35_mod) findcube
contains
  module function cube(x)
    real :: cube
    real, intent(in) :: x
    cube = x**3
  end function cube
end submodule findcube

program submodule_35
  use submodule_35_mod
  implicit none
  if (abs(cube(7.0) - 343.0) > 1.0e-5) error stop
  if (abs(cube(0.0)) > 1.0e-5) error stop
  if (abs(cube(-2.0) - (-8.0)) > 1.0e-5) error stop
  write(*, "(F9.3)") cube(7.0)
end program submodule_35
