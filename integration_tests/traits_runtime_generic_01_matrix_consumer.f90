module traits_runtime_generic_01_consumer_m
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm
    use traits_runtime_generic_01_late_client_m, only: LateValue, PaddedValue, AlternateValue
    implicit none
contains
    integer function invoke(implementation, object) result(r)
        class(IAlgorithm), intent(in) :: implementation
        type(LateValue), intent(in) :: object
        r = implementation%apply(object)
    end function
    integer function invoke_padded(implementation, object) result(r)
        class(IAlgorithm), intent(in) :: implementation
        type(PaddedValue), intent(in) :: object
        r = implementation%apply(object)
    end function
    integer function invoke_alternate(implementation, object) result(r)
        class(IAlgorithm), intent(in) :: implementation
        type(AlternateValue), intent(in) :: object
        r = implementation%apply(object)
    end function
end module
