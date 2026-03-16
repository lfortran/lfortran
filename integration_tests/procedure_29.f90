module procedure_29_iface_m
  implicit none

  abstract interface
    pure function initializer_i(x) result(f)
      double precision, intent(in) :: x(:)
      double precision, allocatable :: f(:)
    end function
  end interface

end module

module procedure_29_functions_m
  implicit none

contains

  pure function f(x)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = x + 1
  end function

end module

program procedure_29
  use procedure_29_iface_m, only: initializer_i
  use procedure_29_functions_m, only: f
  implicit none

  procedure(initializer_i), pointer :: my_ptr => f
  double precision :: res(3)

  res = my_ptr([1d0, 2d0, 3d0])

  if (abs(res(1) - 2d0) > 1d-12) error stop
  if (abs(res(2) - 3d0) > 1d-12) error stop
  if (abs(res(3) - 4d0) > 1d-12) error stop
end program
