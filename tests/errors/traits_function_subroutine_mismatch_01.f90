module traits_function_subroutine_mismatch_01_m
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

    subroutine box_get_value(self)
        class(Box), intent(in) :: self
        print *, self%value
    end subroutine box_get_value
end module traits_function_subroutine_mismatch_01_m
