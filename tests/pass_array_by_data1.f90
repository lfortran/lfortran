module find_fit_module
   implicit none
   public

contains

   subroutine find_fit(expr)
      interface
         function expr(x) result(y)
            implicit none
            real, intent(in) :: x(:)
            real :: y(size(x))
         end function
      end interface

   end subroutine

end module

program main
   use find_fit_module, only: find_fit
   implicit none
   call find_fit(expression)
contains

   function expression(x) result(y)
      real, intent(in) :: x(:)
      real :: y(size(x))
      y = x
   end function

end program
