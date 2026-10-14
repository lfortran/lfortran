module traits_initializers_05_contracts
    implicit none
    private
    public :: IConstructable, IValued, from_int, from_real, valued

    abstract interface :: IConstructable
        initial(n)
            integer, intent(in) :: n
        end initial
        initial(a)
            real, intent(in) :: a
        end initial
    end interface IConstructable

    abstract interface :: IValued
        function value(self) result(r)
            type(itself), intent(in), pass :: self
            real :: r
        end function value
    end interface IValued

contains

    function from_int{IConstructable :: T}(n) result(r)
        integer, intent(in) :: n
        type(T)             :: r
        r = T(n)
    end function from_int

    function from_real{IConstructable :: T}(a) result(r)
        real, intent(in) :: a
        type(T)          :: r
        r = T(a)
    end function from_real

    real function valued(item)
        class(IValued), intent(in) :: item
        valued = item%value()
    end function valued
end module traits_initializers_05_contracts
