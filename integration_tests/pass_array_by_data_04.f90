module find_fit_module
contains
subroutine find_fit(data_x)
real, intent(in) :: data_x(:)
contains
    subroutine fcn()
    end subroutine fcn
end subroutine find_fit

end module find_fit_module

program main

use find_fit_module, only: find_fit

implicit none

real :: data_x(3)
integer :: i
call find_fit(data_x)

end program main
