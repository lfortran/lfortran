module mod1_procedure_18
  implicit none
  private
  public :: f_function

  contains

  elemental function f_function(x, y) result(fx)
    real, intent(in) :: x
    real, intent(in), optional :: y
    real :: w_y
    real :: fx
    w_y=1.0
    if (present(y)) w_y=y
    fx = w_y
  end function f_function
end module mod1_procedure_18

module mod2_procedure_18
  use :: mod1_procedure_18
  implicit none
  private
  public :: function_interface

  interface function_interface
    module procedure f_function
  end interface
end module mod2_procedure_18

program procedure_18
  use :: mod1_procedure_18
  use :: mod2_procedure_18
  implicit none
  real :: res, eps
  res = 0.5
  eps = 1e-8
  print *, "fx is ", f_function(0.8, y=0.5)
  if (abs(f_function(0.8, y=0.5) - res) > eps) error stop
  print *, "fx is ", function_interface(0.8, 0.5)
  if (abs(function_interface(0.8, 0.5) - res) > eps) error stop
  print *, "fx is ", function_interface(0.8, y=0.5)
  if (abs(function_interface(0.8, y=0.5) - res) > eps) error stop
end program procedure_18
