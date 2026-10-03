module generic_interface_dummy_procedure_06_mod_a
implicit none
interface g
  module procedure gi
end interface
contains
  integer function gi(x)
    integer, intent(in) :: x
    gi = x + 1
  end function
end module
module generic_interface_dummy_procedure_06_mod_b
use generic_interface_dummy_procedure_06_mod_a, only: g
implicit none
contains
  real function dbl(x)
    real, intent(in) :: x
    dbl = x * 2
  end function
  subroutine s(v, r)
    integer, intent(inout) :: v
    real, intent(inout) :: r
    integer :: gi
    interface g
      module procedure dbl
    end interface
    gi = 10
    v = g(v) + gi
    r = g(r)
  end subroutine
end module
program generic_interface_dummy_procedure_06
use generic_interface_dummy_procedure_06_mod_b
implicit none
integer :: v
real :: r
v = 1; r = 1.5
call s(v, r)
print *, v, r
if (v /= 12 .or. abs(r-3.0) > 1e-6) error stop
end program
