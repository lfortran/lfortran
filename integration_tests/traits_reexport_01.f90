module traits_reexport_01_source_m
    implicit none
    abstract interface :: IValue
        function value() result(result_value)
            integer :: result_value
        end function
    end interface
    type :: Box
        integer :: data
    end type
    implements IValue :: Box
        procedure, pass :: value => box_value
    end implements
contains
    function box_value(self) result(result_value)
        class(Box), intent(in) :: self
        integer :: result_value
        result_value = self%data
    end function
    function query{IValue :: T}(object) result(result_value)
        type(T), intent(in) :: object
        integer :: result_value
        result_value = object%value()
    end function
end module

module traits_reexport_01_left_m
    use traits_reexport_01_source_m
end module

module traits_reexport_01_right_m
    use traits_reexport_01_source_m, AliasBox => Box
end module

program traits_reexport_01
    use traits_reexport_01_left_m
    use traits_reexport_01_right_m
    implicit none
    type(Box) :: object
    object = Box(31)
    if (query(object) /= 31) error stop
    if (query{AliasBox}(object) /= 31) error stop
end program
