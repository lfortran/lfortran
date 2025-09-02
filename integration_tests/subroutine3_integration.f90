program subroutine3_integration
implicit none
integer :: y
y = f()
if (y /= 42) error stop

contains

    integer function f()
    f = 42
    end function

end program
