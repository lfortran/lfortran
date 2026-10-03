! Test: procedure pointer with abstract interface in a module that also
! contains a "module subroutine" declaration (submodule interface).
! The presence of the module subroutine must not break the LLVM type
! of the procedure pointer.
module procedure_42_iface_m
  implicit none
  abstract interface
    pure function initializer_i(x) result(f)
      double precision, intent(in) :: x(:)
      double precision, allocatable :: f(:)
    end function
  end interface
  interface
    module subroutine s()
    end subroutine
  end interface
end module

module procedure_42_functions_m
  implicit none
contains
  pure function f(x)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    allocate(f(size(x)))
    f = x
  end function
end module

program procedure_42
  use procedure_42_iface_m, only : initializer_i
  use procedure_42_functions_m, only : f
  implicit none
  procedure(initializer_i), pointer :: p
  double precision :: x(3), y(3)
  p => f
  x = [1.0d0, 2.0d0, 3.0d0]
  y = p(x)
  if (abs(y(1) - 1.0d0) > 1.0d-12) error stop
  if (abs(y(2) - 2.0d0) > 1.0d-12) error stop
  if (abs(y(3) - 3.0d0) > 1.0d-12) error stop
  print *, "ok"
end program
