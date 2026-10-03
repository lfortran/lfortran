program submodule_53a
  use submodule_53_mod
  implicit none
  type(t) :: c
  real :: a(3), b(3)
  a = 1.0
  b = 0.0
  c%p => impl
  call c%p(a, b)
  if (any(b /= a)) error stop
end program
