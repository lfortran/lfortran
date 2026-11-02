! With --fast, a module procedure that an internal procedure is passed to is
! copied into the host. A module variable used in a BLOCK of the copy must
! still be the module's variable, not the host's local of the same name.
module procedure_argument_specialization_07_m
implicit none
real :: scale = 3
contains
real function integrate(f, n) result(s)
  interface
    real function f(x)
      real, intent(in) :: x
    end function
  end interface
  integer, intent(in) :: n
  integer :: i
  s = 0
  block
    real :: t
    do i = 1, n
      t = f(real(i))
      s = s + t*scale
    end do
  end block
end function
end module
program procedure_argument_specialization_07
use procedure_argument_specialization_07_m, only: integrate
implicit none
real :: c, scale, r
c = 2
scale = 1000
r = integrate(inner, 3)
print *, r, scale
if (abs(r - 36) > 1e-3) error stop
contains
real function inner(x)
  real, intent(in) :: x
  inner = c*x
end function
end program
