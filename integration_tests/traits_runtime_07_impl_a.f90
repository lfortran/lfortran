module traits_runtime_07_impl_a_m
    use traits_runtime_07_contracts_m, only: IValue
    implicit none
    private
    public :: make_a
    type :: HiddenA
        integer :: payload
    end type HiddenA
    implements IValue :: HiddenA
        procedure, pass :: value => a_value
    end implements HiddenA
contains
    function a_value(self) result(r)
        class(HiddenA), intent(in) :: self
        integer :: r
        r = self%payload
    end function a_value
    subroutine make_a(object)
        class(IValue), allocatable, intent(out) :: object
        type(HiddenA) :: source
        source%payload = 17
        allocate(object, source=source)
    end subroutine make_a
end module traits_runtime_07_impl_a_m
