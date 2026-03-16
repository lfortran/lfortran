module separate_compilation_34a
  implicit none
  abstract interface
    function func_iface(x)
      double precision, intent(in) :: x
      double precision :: func_iface
    end function
  end interface
contains
  function my_func(x) result(y)
    double precision, intent(in) :: x
    double precision :: y
    y = x * 2.0d0
  end function

  subroutine call_it()
    procedure(func_iface), pointer, save :: fptr => my_func
    double precision :: res
    res = fptr(3.0d0)
    print *, res
    if (abs(res - 6.0d0) > 1.0d-12) error stop
  end subroutine
end module separate_compilation_34a
