module traits_separate_01_contracts_m
    implicit none
    abstract interface :: IValue
        function get_value() result(value)
            integer :: value
        end function get_value
    end interface IValue
contains
    function read_value{IValue :: T}(object) result(value)
        type(T), intent(in) :: object
        integer :: value
        value = object%get_value()
    end function read_value
end module traits_separate_01_contracts_m
