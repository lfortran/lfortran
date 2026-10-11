module traits_runtime_generic_01_provider_m
    use traits_runtime_generic_01_contracts_m, only: IValue, IAlgorithm
    implicit none
    private
    public :: build_algorithm
    type :: OffsetAlgorithm
    end type OffsetAlgorithm
    type :: ScaledAlgorithm
    end type ScaledAlgorithm
    implements IAlgorithm :: OffsetAlgorithm
        procedure, nopass :: apply => offset_apply
    end implements OffsetAlgorithm
    implements IAlgorithm :: ScaledAlgorithm
        procedure, nopass :: apply => scaled_apply
    end implements ScaledAlgorithm
contains
    function offset_apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = object%value() + 10
    end function offset_apply
    function scaled_apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = 2 * object%value() + 100
    end function scaled_apply
    subroutine build_algorithm(choice, object)
        integer, intent(in) :: choice
        class(IAlgorithm), allocatable, intent(out) :: object
        type(OffsetAlgorithm) :: offset
        type(ScaledAlgorithm) :: scaled
        if (choice == 0) then
            allocate(object, source=offset)
        else
            allocate(object, source=scaled)
        end if
    end subroutine build_algorithm
end module traits_runtime_generic_01_provider_m

subroutine make_algorithm(choice, object)
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm
    use traits_runtime_generic_01_provider_m, only: build_algorithm
    implicit none
    integer, intent(in) :: choice
    class(IAlgorithm), allocatable, intent(out) :: object
    call build_algorithm(choice, object)
end subroutine make_algorithm
