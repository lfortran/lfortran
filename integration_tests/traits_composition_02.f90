module traits_composition_02_m
    implicit none

    abstract interface :: ILeft
        function value() result(res)
            integer :: res
        end function value
    end interface ILeft

    abstract interface :: IRight
        function value() result(right_result)
            integer :: right_result
        end function value
    end interface IRight

    abstract interface, extends(ILeft + IRight) :: IJoined
    end interface IJoined

    type :: Payload
        integer :: data
    end type Payload

    implements IJoined :: Payload
        procedure, pass :: value => payload_value
    end implements Payload

contains

    function payload_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function left_value{ILeft :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function left_value

    function right_value{IRight :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function right_value

    function combined_value{ILeft + IRight :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = left_value(object) + right_value(object) + object%value()
    end function combined_value

    function joined_value{IJoined :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = combined_value(object)
    end function joined_value

    function redundant_value{IJoined + IRight + ILeft :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = joined_value(object) + object%value()
    end function redundant_value
end module traits_composition_02_m

program traits_composition_02
    use traits_composition_02_m
    implicit none
    type(Payload) :: object

    object = Payload(7)
    if (left_value(object) /= 7) error stop 1
    if (right_value(object) /= 7) error stop 2
    if (combined_value(object) /= 21) error stop 3
    if (joined_value(object) /= 21) error stop 4
    if (redundant_value(object) /= 28) error stop 5
    if (redundant_value{Payload}(object) /= 28) error stop 6
end program traits_composition_02
