module traits_static_03_m
    implicit none

    abstract interface :: IBoxOps
        subroutine report(label, out)
            character(*), intent(in) :: label
            integer, intent(out) :: out
        end subroutine report

        subroutine reset(out)
            integer, intent(out) :: out
        end subroutine reset
    end interface IBoxOps

    type :: Box
        integer :: value
    end type Box

    implements IBoxOps :: Box
        procedure, pass(self) :: report => box_report
        procedure, nopass :: reset => box_reset
    end implements Box

contains

    subroutine box_report(label, self, out)
        character(*), intent(in) :: label
        class(Box), intent(in) :: self
        integer, intent(out) :: out

        if (label == "box") then
            out = self%value + 10
        else
            out = self%value
        end if
    end subroutine box_report

    subroutine box_reset(out)
        integer, intent(out) :: out
        out = 0
    end subroutine box_reset

    subroutine render{IBoxOps :: T}(x, out)
        type(T), intent(in) :: x
        integer, intent(out) :: out
        call x%report("box", out)
    end subroutine render

    subroutine reset_value{IBoxOps :: T}(x, out)
        type(T), intent(in) :: x
        integer, intent(out) :: out
        call x%reset(out)
    end subroutine reset_value
end module traits_static_03_m

program traits_static_03
    use traits_static_03_m
    implicit none
    type(Box) :: object
    integer :: out

    object = Box(7)

    call object%report("box", out)
    if (out /= 17) error stop

    call object%report("other", out)
    if (out /= 7) error stop

    call object%reset(out)
    if (out /= 0) error stop

    call render(object, out)
    if (out /= 17) error stop

    call reset_value(object, out)
    if (out /= 0) error stop
end program traits_static_03
