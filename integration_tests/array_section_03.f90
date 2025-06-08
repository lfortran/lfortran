program array_section_03
   implicit none
   integer :: y(2, 3)
   integer :: x(2)
   y = reshape([6, 3, 8, 5, 2, 7], shape(y))
   x = func(y(:,1))
   print *, size(y(:, 1))
   print *, func(y(:,1))

   if(size(y(:,1)) /= 2) error stop
   if(x(1) /= 6 .or. x(2) /= 3) error stop

contains

 function func(x) result(y)
    integer, intent(in) :: x(:)
    integer :: y(size(x))
    y = x  
  end function func

end program array_section_03