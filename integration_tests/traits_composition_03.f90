module traits_composition_03_left_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue
end module traits_composition_03_left_m

module traits_composition_03_right_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue
end module traits_composition_03_right_m

module traits_composition_03_impl_m
    use traits_composition_03_right_m, only: RightTrait => IValue
    use traits_composition_03_left_m, only: LeftTrait => IValue
    implicit none

    type :: Payload
        integer :: data
    end type Payload

    implements LeftTrait + RightTrait :: Payload
        procedure, pass :: value => payload_value
    end implements Payload

contains

    function payload_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function left_value{LeftTrait :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function left_value

    function right_value{RightTrait :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function right_value

    function sum_values{LeftTrait + RightTrait :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = left_value(object) + right_value(object)
    end function sum_values

    function reversed_values{RightTrait + LeftTrait :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = sum_values(object)
    end function reversed_values
end module traits_composition_03_impl_m

program traits_composition_03
    use traits_composition_03_impl_m, AliasPayload => Payload, query => sum_values
    implicit none
    type(AliasPayload) :: object

    object = AliasPayload(19)
    if (left_value(object) /= 19) error stop 1
    if (right_value(object) /= 19) error stop 2
    if (query(object) /= 38) error stop 3
    if (query{AliasPayload}(object) /= 38) error stop 4
    if (reversed_values(object) /= 38) error stop 5
end program traits_composition_03
