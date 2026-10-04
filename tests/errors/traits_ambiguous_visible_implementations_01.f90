module traits_ambiguous_visible_implementations_01_trait_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

contains

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_value
end module traits_ambiguous_visible_implementations_01_trait_m

module traits_ambiguous_visible_implementations_01_types_m
    implicit none

    type :: Box
        integer :: value
    end type Box
end module traits_ambiguous_visible_implementations_01_types_m

module traits_ambiguous_visible_implementations_01_impl_a_m
    use traits_ambiguous_visible_implementations_01_trait_m, only: IValue, read_value
    use traits_ambiguous_visible_implementations_01_types_m
    implicit none

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value_a
    end implements Box

contains

    function box_get_value_a(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value_a
end module traits_ambiguous_visible_implementations_01_impl_a_m

module traits_ambiguous_visible_implementations_01_impl_b_m
    use traits_ambiguous_visible_implementations_01_trait_m, only: IValue, read_value
    use traits_ambiguous_visible_implementations_01_types_m
    implicit none

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value_b
    end implements Box

contains

    function box_get_value_b(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = 2 * self%value
    end function box_get_value_b
end module traits_ambiguous_visible_implementations_01_impl_b_m

program traits_ambiguous_visible_implementations_01
    use traits_ambiguous_visible_implementations_01_impl_a_m
    use traits_ambiguous_visible_implementations_01_impl_b_m
    implicit none
    type(Box) :: object

    object = Box(7)
    if (read_value(object) /= 7) error stop
end program traits_ambiguous_visible_implementations_01
