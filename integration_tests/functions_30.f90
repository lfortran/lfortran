program functions_30
    real :: x(4) = [1.0, 2.0, 3.0, 4.0]
    print *, maxval([1.0,x])
    if(maxval([1.0,x]) /= 4.0) error stop
    print *, max_func([1.0,x])
    if(max_func([1.0,x]) /= 4.0) error stop
    !! Assignment with function on rhs having lhs var in its arg
    x = x / norm(x)
    print *, x
    if(any(abs(x - [0.182574183, 0.365148365, 0.547722518, 0.730296731]) > 10e-12)) error stop
contains 
    function max_func(x) result(y)
      real, intent(in) :: x(:)
      real :: y
      y = maxval(x)
    end function
    function norm(vec) result(res)
      implicit none
      real, intent(in) :: vec(:)   
      real :: res                  
      integer :: i                 
      res = 0.0                    
      do i = 1, size(vec)
          res = res + vec(i)**2   
      end do
      res = sqrt(res)             
end function norm
end program