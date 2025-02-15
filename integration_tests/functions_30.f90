program functions_30
    real :: x(4) = [1.0, 2.0, -3.0, 4.0]
    integer :: y(4) = [1, 2, -3, 4]

    print *, maxval([1.0,x])
    if(maxval([1.0,x]) /= 4.0) error stop
    print *, max_func([1.0,x])
    if(max_func([1.0,x]) /= 4.0) error stop

    print *, maxval([1.0, -x])
    if(maxval([1.0, -x]) /= 3.0) error stop
    print *, max_func([1.0, -x])
    if(max_func([1.0, -x]) /= 3.0) error stop
    print *, maxval([1.0, x + x([2,1,3,4])])
    if(maxval([1.0, x + x([2,1,3,4])]) /= 8.0) error stop

    print *, maxval([1, -y])
    if(maxval([1, -y]) /= 3) error stop
    print *, maxval([1, y + y([2,1,3,4])])
    if(maxval([1, y + y([2,1,3,4])]) /= 8) error stop
    
    call check_for_args(x, y)
    !! Assignment with function on rhs having lhs var in its arg
    x = x / norm(x)
    print *, x
    if(any(abs(x - [0.182574183, 0.365148365, -0.547722518, 0.730296731]) > 10e-12)) error stop

    x = x / norm(x(1:3))
    print *, x
    if(any(abs(x - [0.267261237, 0.534522474, -0.801783681, 1.06904495]) > 10e-12)) error stop

contains 

    real function max_func(x) result(y)
      real, intent(in) :: x(:)
      y = maxval(x)
    end function

    real function norm(vec) result(res)
      real, intent(in) :: vec(:)   
      integer :: i                 
      res = 0.0                    
      do i = 1, size(vec)
          res = res + vec(i)**2   
      end do
      res = sqrt(res)             
    end function norm

    subroutine check_for_args(x, y)
      real, intent(in) :: x(:)
      integer, intent(in) :: y(:)
      real :: x2(size(x))

      print *, maxval([1.0,x])
      if(maxval([1.0,x]) /= 4.0) error stop
      print *, max_func([1.0,x])
      if(max_func([1.0,x]) /= 4.0) error stop

      print *, maxval([1.0, -x])
      if(maxval([1.0, -x]) /= 3.0) error stop
      print *, max_func([1.0, -x])
      if(max_func([1.0, -x]) /= 3.0) error stop
      print *, maxval([1.0, x + x([2,1,3,4])])
      if(maxval([1.0, x + x([2,1,3,4])]) /= 8.0) error stop

      print *, maxval([1, -y])
      if(maxval([1, -y]) /= 3) error stop
      print *, maxval([1, y + y([2,1,3,4])])
      if(maxval([1, y + y([2,1,3,4])]) /= 8) error stop
      
      x2 = x
      x2 = x / norm(x2)
      print *, x2
      if(any(abs(x2 - [0.182574183, 0.365148365, -0.547722518, 0.730296731]) > 10e-12)) error stop

      x2 = x2 / norm(x2(1:3))
      print *, x2
      if(any(abs(x2 - [0.267261237, 0.534522474, -0.801783681, 1.06904495]) > 10e-12)) error stop

    end subroutine check_for_args
end program