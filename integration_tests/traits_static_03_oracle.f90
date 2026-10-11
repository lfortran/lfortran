module traits_static_03_oracle_m
    implicit none

    type :: Box
        integer :: value
    contains
        procedure, pass(self) :: report => box_report
        procedure, nopass :: reset => box_reset
    end type Box

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

    subroutine render(x, out)
        type(Box), intent(in) :: x
        integer, intent(out) :: out
        call x%report("box", out)
    end subroutine render

    subroutine reset_value(x, out)
        type(Box), intent(in) :: x
        integer, intent(out) :: out
        call x%reset(out)
    end subroutine reset_value
end module traits_static_03_oracle_m

program traits_static_03_oracle
    use traits_static_03_oracle_m
    implicit none
    type(Box) :: obj
    integer :: out

    obj = Box(7)

    call obj%report("box", out)
    if (out /= 17) error stop

    call obj%report("other", out)
    if (out /= 7) error stop

    call obj%reset(out)
    if (out /= 0) error stop

    call render(obj, out)
    if (out /= 17) error stop

    call reset_value(obj, out)
    if (out /= 0) error stop
end program traits_static_03_oracle
