module evaluate_mod
    implicit none
    abstract interface
       function f(x)
         real, intent(in) :: x
         real :: f
       end function f
    end interface
  contains
  
    function cube(x)
      real, intent(in) :: x
      real :: cube
      cube = x**3
    end function cube
  
    function square(x)
      real, intent(in) :: x
      real :: square
      square = x**2
    end function square
  
  end module evaluate_mod
  
  module f_as_arg_mod
    use evaluate_mod, only: f
    implicit none
  contains
  
    function value(x, fx)
      real, intent(in) :: x
      real :: value
      procedure(f) :: fx
      value = fx(x)
    end function value
  
  end module f_as_arg_mod
  
  program procedure_46
    use f_as_arg_mod, only: value
    use evaluate_mod, only: f, square, cube
    implicit none
    real :: sq, cu
  
    sq = value(3.0, square)
    cu = value(3.0, cube)
    if (abs(sq - 9.0) > 1.0e-6) error stop
    if (abs(cu - 27.0) > 1.0e-6) error stop
  end program procedure_46
