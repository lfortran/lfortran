! A generic definition calls a generic trait method with a concrete type
! argument whose conformance is visible in the definition's module only. A
! client that imports just the generic and the implementing type needs no
! conformance of Value (review 01c6aed8).
module traits_generic_requirement_06_m
    implicit none

    abstract interface :: IValue
        function value(self) result(r)
            type(itself), intent(in), pass :: self
            integer :: r
        end function value
    end interface IValue

    abstract interface :: IApply
        function apply{IValue :: T}(self, object) result(r)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: object
            integer :: r
        end function apply
    end interface IApply

    type, implements(IValue) :: Value
        integer :: n = 5
    contains
        procedure, pass :: value => value_of
    end type Value

    type, implements(IApply) :: Offset
        integer :: n = 7
    contains
        procedure, pass(self) :: apply => offset_apply
    end type Offset

contains

    integer function value_of(self)
        class(Value), intent(in) :: self
        value_of = self%n
    end function value_of

    function offset_apply{IValue :: Element}(self, object) result(r)
        class(Offset), intent(in) :: self
        type(Element), intent(in) :: object
        integer :: r
        r = self%n + object%value()
    end function offset_apply

    function g{IApply :: A}(alg) result(r)
        type(A), intent(in) :: alg
        integer :: r
        r = alg%apply(Value())
    end function g

end module traits_generic_requirement_06_m

program traits_generic_requirement_06
    use traits_generic_requirement_06_m, only: g, Offset
    implicit none

    if (g(Offset()) /= 12) error stop 1
    print '(a)', 'traits_generic_requirement_06 ok'
end program traits_generic_requirement_06
