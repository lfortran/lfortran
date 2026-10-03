module generic_interface_dummy_procedure_04_mod
implicit none
interface g
  module procedure gi
end interface
contains
  integer function gi(x)
    integer, intent(in) :: x
    gi = x + 1
  end function
  real function dbl(x)
    real, intent(in) :: x
    dbl = x * 2
  end function
  subroutine s(v, r)
    integer, intent(inout) :: v
    real, intent(inout) :: r
    interface g
      module procedure dbl
    end interface
    v = g(v)
    r = g(r)
    v = v + gi(0)
  contains
    integer function gi(x)
      integer, intent(in) :: x
      gi = x + 100
    end function
  end subroutine
end module
program generic_interface_dummy_procedure_04
use generic_interface_dummy_procedure_04_mod
implicit none
integer :: v
real :: r
v = 1; r = 1.5
call s(v, r)
print *, v, r
if (v /= 102 .or. abs(r-3.0) > 1e-6) error stop
end program
