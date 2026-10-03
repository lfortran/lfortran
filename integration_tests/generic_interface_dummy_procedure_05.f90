module generic_interface_dummy_procedure_05_mod
implicit none
interface g
  procedure gi
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
    integer :: gi
    interface g
      module procedure dbl
    end interface
    gi = 0
    v = g(v) + gi
    r = g(r)
  end subroutine
end module
program generic_interface_dummy_procedure_05
use generic_interface_dummy_procedure_05_mod
implicit none
integer :: v
real :: r
v = 1; r = 1.5
call s(v, r)
print *, v, r
if (v /= 2 .or. abs(r-3.0) > 1e-6) error stop
end program
