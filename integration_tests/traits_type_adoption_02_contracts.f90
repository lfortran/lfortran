module traits_type_adoption_02_contracts
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: IExtra
        integer function extra(self, n)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: n
        end function
    end interface
    abstract interface, extends(IValue + IExtra) :: IAll
        real function measure(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
end module
