program generic_interface_dummy_procedure_03
implicit none
interface g
  procedure gi
end interface
integer :: v
real :: r
v = 1; r = 1.5
call s(dbl, v, r)
print *, v, r
if (v /= 2 .or. abs(r-3.0) > 1e-6) error stop
contains
  integer function gi(x)
    integer, intent(in) :: x
    gi = x + 1
  end function
  real function dbl(x)
    real, intent(in) :: x
    dbl = x * 2
  end function
  subroutine s(f, v, r)
    interface
      real function f(x)
        real, intent(in) :: x
      end function
    end interface
    integer, intent(inout) :: v
    real, intent(inout) :: r
    interface g
      procedure f
    end interface
    v = g(v)
    r = g(r)
  end subroutine
end program generic_interface_dummy_procedure_03
