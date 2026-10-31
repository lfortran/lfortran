! A procedure pointer passed as the actual argument of a dummy procedure
! declared by an interface body: the callee receives the pointer's target.
module procedure_pointer_42_m
implicit none
contains
integer function call_it(f)
    interface
        integer function f()
        end function
    end interface
    call_it = f()
end function

subroutine call_sub(s, x)
    interface
        subroutine s(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
    integer, intent(inout) :: x
    call s(x)
end subroutine

integer function seven()
    seven = 7
end function

subroutine add_three(x)
    integer, intent(inout) :: x
    x = x + 3
end subroutine
end module

program procedure_pointer_42
use procedure_pointer_42_m
implicit none
procedure(seven), pointer :: p
procedure(add_three), pointer :: q
integer :: x
p => seven
if (call_it(p) /= 7) error stop 1
q => add_three
x = 1
call call_sub(q, x)
if (x /= 4) error stop 2
print *, call_it(p), x
end program
