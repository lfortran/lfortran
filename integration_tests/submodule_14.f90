module the_interface_submodule_14
  implicit none
  integer,parameter:: dp = selected_real_kind(15)
  interface
     real(dp) module function pi()
     end function pi
  end interface
end module the_interface_submodule_14

submodule (the_interface_submodule_14) the_calculation_submodule_14
  implicit none
contains
  real(dp) module function pi()
    pi = acos(-1.0_dp)
  end function pi
end submodule the_calculation_submodule_14

program submodule_14
  use the_interface_submodule_14
  implicit none
  real(dp) :: result = 1.0_dp
  result = pi()
  print *, result
  if (result /= acos(-1.0_dp)) error stop
end program