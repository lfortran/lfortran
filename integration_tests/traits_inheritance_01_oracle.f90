module traits_inheritance_01_oracle_m
    implicit none

    type :: Payload
        integer :: data
    contains
        procedure :: get_value => payload_value
        procedure :: shift => payload_shift
        procedure :: report => payload_report
    end type Payload

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

    function read_value(object) result(res)
        type(Payload), intent(in) :: object
        integer :: res
        res = object%get_value()
    end function read_value

    function shifted_value(object) result(res)
        type(Payload), intent(in) :: object
        integer :: res
        res = read_value(object) + object%shift(3)
    end function shifted_value

    function total_value(object) result(res)
        type(Payload), intent(in) :: object
        integer :: res, out
        call object%report(out)
        res = shifted_value(object) + out
    end function total_value
end module traits_inheritance_01_oracle_m

program traits_inheritance_01_oracle
    use traits_inheritance_01_oracle_m
    implicit none
    type(Payload) :: object

    object = Payload(7)
    if (object%get_value() /= 7) error stop 1
    if (read_value(object) /= 7) error stop 2
    if (shifted_value(object) /= 17) error stop 3
    if (total_value(object) /= 31) error stop 4

    object = Payload(2)
    if (read_value(object) /= 2) error stop 5
    if (shifted_value(object) /= 7) error stop 6
    if (total_value(object) /= 11) error stop 7
end program traits_inheritance_01_oracle
