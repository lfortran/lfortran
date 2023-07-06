module template_error_07_m
    implicit none
    private
    public :: tmp

    requirement r(t, f)
        type, deferred :: t
        function f(x, y) result(z)
            type(t), intent(in) :: x, y
            type(t) :: z
        end function
    end requirement

    template tmp(t, f)
        requires r(t)
    end template

contains
  
end module

program template_error_07
use template_error_07_m
implicit none

end program template_error_07
