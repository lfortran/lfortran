program functions_30
    real :: x(4) = [1.0, 2.0, 3.0, 4.0]
    print *, maxval([1.0,x])
    if(maxval([1.0,x]) /= 4.0) error stop
    print *, max_func([1.0,x])
    if(max_func([1.0,x]) /= 4.0) error stop
contains 
    function max_func(x) result(y)
      real, intent(in) :: x(:)
      real :: y
      y = maxval(x)
    end function
end program