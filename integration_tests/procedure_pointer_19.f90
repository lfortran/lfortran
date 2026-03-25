program procedure_pointer_19
    implicit none
    type :: t
        procedure(), pointer, nopass :: caller => null()
        procedure(), pointer, nopass :: f => null()
    end type
    type(t) :: x
    x%caller => sub
    x%f => sub
    call x%caller(x%f)
    print *, "ok"
contains
    subroutine sub(func)
        procedure() :: func
    end subroutine
end program
