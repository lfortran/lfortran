module template_array_04b_m
    implicit none
    private
    public :: test_reverse

    requirement default_behavior(t)
        type, deferred :: t
    end requirement

contains

    subroutine swap{T}(x, y)
        requires default_behavior(T)
        type(T), intent(inout) :: x, y
        type(T) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine
    
    subroutine test_reverse()
        integer :: a(2)
        a = [1,2]
        print *, a
        call swap{integer}(a(1),a(2))
        print *, a
    end subroutine

end module

program template_array_04b
use template_array_04b_m

call test_reverse()

end program
