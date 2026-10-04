module traits_structural_no_nominal_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    contains
        procedure, pass :: get_value => box_get_value
    end type Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_value

end module traits_structural_no_nominal_01_m

program traits_structural_no_nominal_01
    use traits_structural_no_nominal_01_m
    implicit none
    type(Box) :: object
    integer :: value
    object = Box(3)
    value = read_value(object)
end program traits_structural_no_nominal_01
