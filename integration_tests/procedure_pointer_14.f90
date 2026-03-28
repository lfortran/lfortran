program procedure_pointer_14
    implicit none

    type :: t
        procedure(ifc), pointer :: ptr => null()
    end type t

    abstract interface
        subroutine ifc(self, a)
            import
            class(t), intent(in) :: self
            integer, optional, intent(in) :: a(:)
        end subroutine ifc
    end interface

    type(t) :: x
    integer :: a(3)
    a = [10, 20, 30]
    x%ptr => sub
    call x%ptr(a)

contains

    subroutine sub(self, a)
        class(t), intent(in) :: self
        integer, optional, intent(in) :: a(:)
        if (.not. present(a)) error stop
        if (size(a) /= 3) error stop
        if (a(1) /= 10) error stop
        if (a(2) /= 20) error stop
        if (a(3) /= 30) error stop
        print *, "OK"
    end subroutine sub

end program procedure_pointer_14
