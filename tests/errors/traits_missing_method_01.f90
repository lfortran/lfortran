module traits_missing_method_01_m
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
    end implements Box
end module traits_missing_method_01_m
