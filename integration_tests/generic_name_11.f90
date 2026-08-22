! A self-named generic whose specific is a *module* procedure: the interface
! body is stored as "set_value~genericprocedure", but it is reached through
! LFortran's mangling of that key, not through an external symbol, so it must
! not carry a Function.link_name.
module generic_name_11_mod
 implicit none
 integer :: last_int = 0
 real :: last_real = 0.0
 interface set_value
  module subroutine set_value(x)
   integer, intent(in) :: x
  end subroutine set_value
  module subroutine set_value_r(r)
   real, intent(in) :: r
  end subroutine set_value_r
 end interface
end module generic_name_11_mod

submodule (generic_name_11_mod) generic_name_11_submod
contains
 module subroutine set_value(x)
  integer, intent(in) :: x
  last_int = x + 1
 end subroutine set_value
 module subroutine set_value_r(r)
  real, intent(in) :: r
  last_real = r + 0.5
 end subroutine set_value_r
end submodule generic_name_11_submod

program generic_name_11
 use generic_name_11_mod
 implicit none
 call set_value(3)
 print *, last_int
 if (last_int /= 4) error stop
 call set_value(1.5)
 print *, last_real
 if (abs(last_real - 2.0) > 1e-6) error stop
 print *, "PASS"
end program generic_name_11
