module the_interface
  implicit none
  integer, parameter :: dp = selected_real_kind(15)
  interface
     real(dp) module function pi() result(p)
     end function pi
  end interface
end module the_interface

submodule (the_interface) the_calculation
  implicit none
contains
  real(dp) module function pi() result(p)
    p = acos(-1.0_dp)
  end function pi
end submodule the_calculation

program submodule_45
  use the_interface
  implicit none
  real(dp) :: val
  val = pi()
  if (abs(val - 3.141592653589793_dp) > 1.0e-10_dp) error stop
end program submodule_45
