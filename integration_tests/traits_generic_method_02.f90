! Unused checked generics and erased-entry metadata do not require runtime lowering.
module traits_generic_method_02_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: Renamed}(object) result(r)
        type(Renamed), intent(in) :: object
        integer :: r
        r = object%value() + 10
    end function
end module

program traits_generic_method_02
    use traits_generic_method_02_m
    implicit none
    integer :: n
    n = 4
    if (n /= 4) error stop 1
end program
