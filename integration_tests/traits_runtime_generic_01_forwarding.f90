! Checked and serialized before the late concrete argument types exist.
module traits_runtime_generic_forwarding_m
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm, IValue
    implicit none
contains
    function forward{IValue :: T}(implementation, object) result(r)
        class(IAlgorithm), intent(in) :: implementation
        type(T), intent(in) :: object
        integer :: r
        r = implementation%apply(object)
    end function
    function forward_twice{IValue :: T}(implementation, object) result(r)
        class(IAlgorithm), intent(in) :: implementation
        type(T), intent(in) :: object
        integer :: r
        r = forward(implementation, object) + forward{T}(implementation, object)
    end function
end module
