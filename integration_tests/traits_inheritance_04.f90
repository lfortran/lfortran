module traits_inheritance_04_contracts_m
    implicit none

    abstract interface :: IBase
        function shift(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shift
        subroutine reset(out)
            integer, intent(out) :: out
        end subroutine reset
    end interface IBase

    abstract interface, extends(IBase) :: IChild
    end interface IChild

contains

    function base_value{IBase :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res, out
        call object%reset(out)
        res = object%shift(4) + out
    end function base_value

    function child_value{IChild :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = base_value(object) + object%shift(1)
    end function child_value

    function both_values{IChild + IBase :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = child_value(object) + base_value(object)
    end function both_values
end module traits_inheritance_04_contracts_m

module traits_inheritance_04_types_m
    implicit none

    type :: Payload
        integer :: data
    end type Payload

contains

    function payload_shift(delta, self) result(res)
        integer, intent(in) :: delta
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data + delta
    end function payload_shift

    subroutine payload_reset(out)
        integer, intent(out) :: out
        out = 5
    end subroutine payload_reset
end module traits_inheritance_04_types_m

module traits_inheritance_04_parent_impl_m
    use traits_inheritance_04_contracts_m, only: IBase, base_value
    use traits_inheritance_04_types_m
    implicit none

    implements IBase :: Payload
        procedure, pass(self) :: shift => payload_shift
        procedure, nopass :: reset => payload_reset
    end implements Payload
end module traits_inheritance_04_parent_impl_m

module traits_inheritance_04_child_impl_m
    use traits_inheritance_04_contracts_m, only: IChild, child_value, both_values
    use traits_inheritance_04_types_m, RenamedPayload => Payload, &
        shifted => payload_shift, resetter => payload_reset
    implicit none

    implements IChild :: RenamedPayload
        procedure, nopass :: reset => resetter
        procedure, pass(self) :: shift => shifted
    end implements RenamedPayload
end module traits_inheritance_04_child_impl_m

module traits_inheritance_04_left_m
    use traits_inheritance_04_parent_impl_m
    use traits_inheritance_04_child_impl_m
    implicit none
contains
    function left_result(object) result(res)
        type(Payload), intent(in) :: object
        integer :: res
        res = base_value(object) + child_value(object) + both_values(object)
    end function left_result
end module traits_inheritance_04_left_m

module traits_inheritance_04_right_m
    use traits_inheritance_04_child_impl_m
    use traits_inheritance_04_parent_impl_m
    implicit none
contains
    function right_result(object) result(res)
        type(RenamedPayload), intent(in) :: object
        integer :: res
        res = both_values{RenamedPayload}(object) + child_value(object) + base_value(object)
    end function right_result
end module traits_inheritance_04_right_m

program traits_inheritance_04
    use traits_inheritance_04_left_m
    use traits_inheritance_04_right_m
    implicit none
    type(RenamedPayload) :: object
    integer :: out

    object = RenamedPayload(13)
    if (base_value(object) /= 22) error stop 1
    if (child_value(object) /= 36) error stop 2
    if (both_values(object) /= 58) error stop 3
    if (left_result(object) /= 116) error stop 4
    if (right_result(object) /= 116) error stop 5
    if (object%shift(4) /= 17) error stop 6
    call object%reset(out)
    if (out /= 5) error stop 7
end program traits_inheritance_04
