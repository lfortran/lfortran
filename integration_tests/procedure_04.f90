module procedure_04_mod

interface
    subroutine func(n)
    integer, intent(inout) :: n
    end subroutine
end interface

contains

subroutine g(fcn2, n)
procedure(func) :: fcn2
integer, intent(inout) :: n
call fcn2(n)
end subroutine

subroutine f(fcn1, n)
procedure(func) :: fcn1
integer, intent(inout) :: n
call g(fcn1, n)
end subroutine

end module

program procedure_04
use procedure_04_mod, only: f
integer :: n

n = 5
call f(myfn, n)
print *, n
if (n /= 6) error stop

contains

    subroutine myfn(n)
    integer, intent(inout) :: n
    n = n + 1
    end subroutine

end program
