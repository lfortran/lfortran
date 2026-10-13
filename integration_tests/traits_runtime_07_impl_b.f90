module traits_runtime_07_impl_b_m
    use traits_runtime_07_contracts_m, only: IValue
    implicit none
    private
    public :: make_b
    type :: HiddenB
        integer :: factor, payload
    end type HiddenB
    implements IValue :: HiddenB
        procedure, pass :: value => b_value
    end implements HiddenB
contains
    function b_value(self) result(r)
        class(HiddenB), intent(in) :: self
        integer :: r
        r = self%factor * self%payload + 1
    end function b_value
    subroutine make_b(object)
        class(IValue), allocatable, intent(out) :: object
        type(HiddenB) :: source
        source%factor = 4
        source%payload = 7
        allocate(object, source=source)
    end subroutine make_b
end module traits_runtime_07_impl_b_m
