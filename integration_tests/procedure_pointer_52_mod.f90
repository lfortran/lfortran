module procedure_pointer_52_mod
    implicit none
    abstract interface
        subroutine iface(a, b, c, d, e)
            integer, intent(in) :: a, b, c, d
            integer, intent(in), optional :: e
        end subroutine
    end interface
    type :: t
        procedure(iface), pointer, nopass :: f => null()
    end type
contains
    subroutine set(me, f)
        type(t), intent(inout) :: me
        procedure(iface) :: f
        me%f => f
    end subroutine
    subroutine impl(a, b, c, d, e)
        integer, intent(in) :: a, b, c, d
        integer, intent(in), optional :: e
        if (.not. present(e)) error stop 2
        if (a + b + c + d + e /= 15) error stop 1
    end subroutine
end module
