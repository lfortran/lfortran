module traits_runtime_unimplemented_01_m
    implicit none
    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function
    end interface
    ! This guard is temporary until runtime trait objects are implemented.
    class(IValue), allocatable :: object
end module
