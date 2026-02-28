! Test passing a function name directly to a procedure-pointer dummy argument.
! The callee declares 'procedure(...), pointer, intent(in)' but the actual
! argument is the bare subroutine name (not a pointer variable).
program procedure_32
    implicit none

    abstract interface
        subroutine intfc()
        end subroutine intfc
    end interface

    integer :: x
    x = 0
    call output(hello)
    if (x /= 1) error stop

contains

    subroutine output(ptr)
        procedure(intfc), pointer, intent(in) :: ptr
        call ptr()
    end subroutine output

    subroutine hello()
        x = 1
    end subroutine hello

end program procedure_32
