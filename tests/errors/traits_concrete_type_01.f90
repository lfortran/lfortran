module traits_concrete_type_01_m
    implicit none
    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function
    end interface
    type(IValue) :: object
end module
