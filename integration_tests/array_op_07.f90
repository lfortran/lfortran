program array_op_07
    implicit none
  
    ! Declare variables
    integer, dimension(5) :: array
    real :: scalar_value
    integer :: i
    real, dimension(5) :: x, y, z
  
    array = (/ 1, 0, 3, 0, 5 /)
    x = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)
    y = (/ 2.0, 4.0, 6.0, 8.0, 10.0 /)
    
    scalar_value = 2.5
    
    print *, "Result:", square(scalar_value)
    if ((abs(square(scalar_value) - 6.25000000e+00)) > 1e-12) error stop
  
    z = array_function(x, y)
    print *, "Result (z):", z
    if (any(abs(z - 0.0) > 1e-12)) error stop
  
    where (array == 0)
      array = 10
    end where
  
    do i = 1, size(array)
      print *, "array(", i, ") squared =", square(real(array(i)))
    end do
  
  contains
  
    real function square(a)
      real, intent(in) :: a
      square = a * a
    end function
  
    real function array_function(a, b)
      real, intent(in) :: a(:), b(:)
      real :: temp(size(a))
      array_function = 0.0
  
      temp = a + sqrt(b)
      array_function = temp(0)
    end function
  
end program array_op_07
  