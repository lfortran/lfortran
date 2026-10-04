module traits_bad_body_undeclared_method_01_m
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
        procedure, pass :: get_value => box_get_value
    end implements Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value

    function bad_read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%missing_value()
    end function bad_read_value
end module traits_bad_body_undeclared_method_01_m
