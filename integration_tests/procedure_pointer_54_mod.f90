module procedure_pointer_54_mod
    implicit none
    abstract interface
        subroutine iface(x, y, flag)
            real, intent(in) :: x(:)
            real, intent(in) :: y(:)
            integer, intent(in), optional :: flag
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
    subroutine impl(x, y, flag)
        real, intent(in) :: x(:), y(:)
        integer, intent(in), optional :: flag
        if (.not. present(flag)) error stop 2
        if (abs(sum(x) + sum(y) + flag - 5.0) > 1.0e-5) error stop 1
    end subroutine
    subroutine run(me)
        type(t), intent(inout) :: me
        real :: x(2), y(2)
        x = 1.0
        y = 1.0
        call me%f(x, y, 1)
    end subroutine
end module
