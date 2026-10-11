module traits_inheritance_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    abstract interface, extends(IValue) :: IShift
        function shift(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shift
    end interface IShift

    abstract interface, extends(IShift) :: IReport
        subroutine report(out)
            integer, intent(out) :: out
        end subroutine report
    end interface IReport

    type :: Payload
        integer :: data
    end type Payload

    implements IReport :: Payload
        procedure, pass :: report => payload_report
        procedure, pass :: shift => payload_shift
        procedure, pass :: get_value => payload_value
    end implements Payload

contains

    function payload_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function payload_shift(self, delta) result(res)
        class(Payload), intent(in) :: self
        integer, intent(in) :: delta
        integer :: res
        res = self%data + delta
    end function payload_shift

    subroutine payload_report(self, out)
        class(Payload), intent(in) :: self
        integer, intent(out) :: out
        out = 2 * self%data
    end subroutine payload_report

    function read_value{IValue :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%get_value()
    end function read_value

    function shifted_value{IShift :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = read_value(object) + object%shift(3)
    end function shifted_value

    function total_value{IReport :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res, out
        call object%report(out)
        res = shifted_value(object) + out
    end function total_value
end module traits_inheritance_01_m

program traits_inheritance_01
    use traits_inheritance_01_m
    implicit none
    type(Payload) :: object

    object = Payload(7)
    if (object%get_value() /= 7) error stop 1
    if (read_value(object) /= 7) error stop 2
    if (shifted_value(object) /= 17) error stop 3
    if (total_value(object) /= 31) error stop 4
    if (total_value{Payload}(object) /= 31) error stop 5

    object = Payload(2)
    if (read_value{Payload}(object) /= 2) error stop 6
    if (shifted_value{Payload}(object) /= 7) error stop 7
    if (total_value(object) /= 11) error stop 8
end program traits_inheritance_01
