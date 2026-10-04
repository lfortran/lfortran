module traits_private_binding_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, private :: get_value => box_get_value
    end implements Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value
end module traits_private_binding_01_m
