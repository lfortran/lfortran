module procedure_01_module
implicit none

abstract interface
    integer function fn(n)
    integer, intent(in) :: n
    end function
end interface

contains

    integer function plus(f, x)
    procedure(fn) :: f
    integer, intent(in) :: x
    plus = f(x+4)
    end function

end module

program procedure_01
use procedure_01_module, only: plus
implicit none

integer :: i

i = plus(myf, 5)

print *, i

if (i /= 18) error stop

contains


    integer function myf(n)
    integer, intent(in) :: n
    myf = 2*n
    end function

end program
