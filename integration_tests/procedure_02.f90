module procedure_02_module
implicit none

abstract interface
    subroutine fn(n, y)
    integer, intent(in) :: n
    integer, intent(out) :: y
    end subroutine
end interface

contains

    integer function plus(f, x)
    procedure(fn) :: f
    integer, intent(in) :: x
    call f(x+4, plus)
    end function

end module

program procedure_02
use procedure_02_module, only: plus
implicit none

integer :: i

i = plus(myf, 5)

print *, i

if (i /= 18) error stop

contains

    subroutine myf(n, y)
    integer, intent(in) :: n
    integer, intent(out) :: y
    y = 2*n
    end subroutine

end program
