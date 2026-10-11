module traits_runtime_generic_03_m
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm
    use traits_runtime_generic_03_ops_m, only: implementation => transform
    implicit none
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply => implementation
    end implements
end module

program traits_runtime_generic_03
    use traits_runtime_generic_03_m
    use traits_runtime_generic_01_late_client_m, only: LateValue
    implicit none
    type(Algorithm) :: provider
    type(LateValue) :: object
    class(IAlgorithm), allocatable :: owner
    object%payload = 37
    allocate(owner, source=provider)
    if (owner%apply(object) /= 47) error stop 1
    if (provider%apply{LateValue}(object) /= 47) error stop 2
    deallocate(owner)
end program
