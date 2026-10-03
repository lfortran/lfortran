! Test: reshape in submodule body should not cause linker error
module submodule_36_m
  implicit none
  interface
    module subroutine sub(a, b)
      real, intent(in) :: a(:)
      real, intent(out) :: b
    end subroutine
  end interface
end module

submodule(submodule_36_m) submodule_36_s
contains
  module subroutine sub(a, b)
    real, intent(in) :: a(:)
    real, intent(out) :: b
    real :: c(1, size(a))
    c = reshape(a, [1, size(a)])
    b = c(1, 1)
  end subroutine
end submodule

program submodule_36
  use submodule_36_m
  implicit none
  real :: a(3), b
  a = 1.0
  call sub(a, b)
  if (abs(b - 1.0) > 1e-6) error stop
  print *, b
end program
