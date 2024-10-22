program array_section_03
   implicit none
   integer :: y(2, 3)
   y = reshape([6, 3, 8, 5, 2, 7], shape(y))
   print *, size(y(:, 1))
   print *, func(y(:,1))

contains

 function func(x) result(y)
    integer, intent(in) :: x(:)
    integer :: y(size(x))
    y = x  
  end function func

end program array_section_03