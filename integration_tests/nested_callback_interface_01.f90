module my_module
  implicit none

  abstract interface
    pure real function fun_interface()
    end function fun_interface
  end interface
contains
  real function my_wrapper(fun)
    implicit none
    procedure(fun_interface) :: fun

    my_wrapper = wrapper_wrapper()
  contains
    real function wrapper_wrapper()
      wrapper_wrapper = fun()
    end function wrapper_wrapper
  end function my_wrapper
end module my_module

program example
  use my_module
  implicit none

  real :: target_value
  target_value = 0.5

  print *, "Via wrapper (should equal .5): ", my_wrapper(my_fun)
  print *, "Direct call (should equal .5): ", my_fun()

  if (abs(my_wrapper(my_fun) - 0.5) > 1e-6) error stop
  if (abs(my_fun() - 0.5) > 1e-6) error stop
contains
  pure real function my_fun()
    my_fun = target_value
  end function my_fun

end program example
