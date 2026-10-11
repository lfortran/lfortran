module traits_inheritance_05_contracts_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue

    abstract interface :: IScale
        function scaled(factor) result(res)
            integer, intent(in) :: factor
            integer :: res
        end function scaled
    end interface IScale

    abstract interface, extends(IValue + IScale) :: IChild
    end interface IChild

contains

    function read_value{IValue :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function read_value

    function query{IChild + IValue :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = read_value(object) + object%scaled(3)
    end function query
end module traits_inheritance_05_contracts_m
