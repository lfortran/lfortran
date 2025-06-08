module module_mod_59
    implicit none
    private
    public :: is_finite
    interface is_finite
        module procedure is_finite_1
    end interface is_finite

    contains
    
    subroutine is_finite_1(x)
        integer , intent(in) :: x
        print *, x
        if(x /= 1111) error stop
    end subroutine is_finite_1
end module module_mod_59
    
module mod_middle
use, non_intrinsic :: module_mod_59, only : is_finite
end module mod_middle
    
    
program module_59
    real :: x(2)
    call calfun(x)
    contains
    subroutine calfun(x)
        use, non_intrinsic :: mod_middle, only : is_finite
        real, intent(in) :: x(:)
        call is_finite(1111)
    end subroutine calfun
end program module_59