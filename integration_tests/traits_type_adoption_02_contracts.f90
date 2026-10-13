module traits_type_adoption_02_contracts
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IExtra
        integer function extra(n)
            integer, intent(in) :: n
        end function
    end interface
    abstract interface, extends(IValue + IExtra) :: IAll
        real function measure()
        end function
    end interface
end module
