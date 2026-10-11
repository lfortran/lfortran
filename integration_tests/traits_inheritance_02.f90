module traits_inheritance_02_m
    implicit none

    abstract interface :: IShift
        function shift(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shift
    end interface IShift

    abstract interface :: IReport
        subroutine report(delta, out)
            integer, intent(in) :: delta
            integer, intent(out) :: out
        end subroutine report
        subroutine reset(out)
            integer, intent(out) :: out
        end subroutine reset
    end interface IReport

    abstract interface, extends(IShift + IReport) :: ICombined
    end interface ICombined

    type :: Payload
        integer :: data
    end type Payload

    implements ICombined :: Payload
        procedure, nopass :: reset => reset_value
        procedure, pass(self) :: report => payload_report
        procedure, pass(self) :: shift => payload_shift
    end implements Payload

contains

    function payload_shift(amount, self) result(res)
        integer, intent(in) :: amount
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data + amount
    end function payload_shift

    subroutine payload_report(delta, self, out)
        integer, intent(in) :: delta
        class(Payload), intent(in) :: self
        integer, intent(out) :: out
        out = self%data + 2 * delta
    end subroutine payload_report

    subroutine reset_value(out)
        integer, intent(out) :: out
        out = 5
    end subroutine reset_value

    function shifted_value{IShift :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%shift(4)
    end function shifted_value

    function reported_value{IReport :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res, out
        call object%report(3, res)
        call object%reset(out)
        res = res + out
    end function reported_value

    function combined_value{ICombined :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = shifted_value(object) + reported_value(object)
    end function combined_value
end module traits_inheritance_02_m

program traits_inheritance_02
    use traits_inheritance_02_m
    implicit none
    type(Payload) :: object
    integer :: out

    object = Payload(11)
    if (object%shift(4) /= 15) error stop 1
    call object%report(3, out)
    if (out /= 17) error stop 2
    call object%reset(out)
    if (out /= 5) error stop 3
    if (shifted_value(object) /= 15) error stop 4
    if (reported_value(object) /= 22) error stop 5
    if (combined_value(object) /= 37) error stop 6
    if (combined_value{Payload}(object) /= 37) error stop 7
end program traits_inheritance_02
