module procedure_pointer_53_mod
    implicit none
    abstract interface
        subroutine iface(a, b)
            integer, intent(in) :: a
            integer, intent(in), optional :: b
        end subroutine
    end interface
    type :: t
        procedure(iface), pointer, nopass :: f => null()
    end type
contains
    subroutine set(me, f)
        type(t), intent(inout) :: me
        procedure(iface), optional :: f
        if (present(f)) then
            me%f => f
        else
            me%f => null()
        end if
    end subroutine
    subroutine impl(a, b)
        integer, intent(in) :: a
        integer, intent(in), optional :: b
        if (.not. present(b)) error stop 2
        if (a + b /= 3) error stop 1
    end subroutine
end module
