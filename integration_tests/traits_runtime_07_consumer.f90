module traits_runtime_07_consumer_m
    use traits_runtime_07_contracts_m, only: IValue
    implicit none
contains
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function observe
end module traits_runtime_07_consumer_m
