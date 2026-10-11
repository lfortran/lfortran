! Generic procedures of traits_generic_requirement_03 that call generic trait
! methods through their type parameters, compiled separately from both the
! conformances and the clients. Only the contracts are imported.
module traits_generic_requirement_03_generic
    use traits_generic_requirement_03_contracts, only: ISum, INumeric, IApply, IValue
    implicit none
    private
    public :: twice_sum, forward_sum, apply_twice
contains
    function twice_sum{ISum :: Adder, INumeric :: Elem}(a, x) result(s)
        type(Adder), intent(in) :: a
        type(Elem),  intent(in) :: x(:)
        type(Elem)              :: s
        s = a%sum(x) + a%sum(x)
    end function twice_sum

    function forward_sum{ISum :: Summer, INumeric :: Item}(a, x) result(s)
        type(Summer), intent(in) :: a
        type(Item),   intent(in) :: x(:)
        type(Item)               :: s
        s = twice_sum(a, x) + a%sum{Item}(x)
    end function forward_sum

    function apply_twice{IApply :: Algorithm, IValue :: V}(alg, object) result(r)
        type(Algorithm), intent(in) :: alg
        type(V),         intent(in) :: object
        integer :: r
        r = alg%apply(object) + alg%apply{V}(object)
    end function apply_twice
end module traits_generic_requirement_03_generic
