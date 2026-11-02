! With --fast, a module procedure that an internal procedure is passed to is
! copied into the host. A module variable and a module procedure used in an
! ASSOCIATE of the copy must still be the module's entities, not the host's
! local and internal procedure of the same names.
module procedure_argument_specialization_08_m
implicit none
real :: scale = 3
contains
real function helper(x)
  real, intent(in) :: x
  helper = x + 1
end function
real function integrate(f, n) result(s)
  interface
    real function f(x)
      real, intent(in) :: x
    end function
  end interface
  integer, intent(in) :: n
  integer :: i
  s = 0
  do i = 1, n
    associate (t => f(real(i)))
      s = s + helper(t)*scale
    end associate
  end do
end function
end module
program procedure_argument_specialization_08
use procedure_argument_specialization_08_m, only: integrate
implicit none
real :: c, scale, r
c = 2
scale = 1000
r = integrate(inner, 3)
print *, r
if (abs(r - 45) > 1e-3) error stop
contains
real function inner(x)
  real, intent(in) :: x
  inner = c*x
end function
real function helper(x)
  real, intent(in) :: x
  helper = -x
end function
end program
