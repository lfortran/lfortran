! Elemental trait messages applied to arrays across separately compiled
! modules: the consumer is compiled without the provider of the conforming
! type.
program traits_runtime_elemental_separate_01
    use traits_runtime_elemental_separate_01_contracts_m, only: IFloors
    use traits_runtime_elemental_separate_01_provider_m, only: Floors, make_floors
    use traits_runtime_elemental_separate_01_consumer_m, only: Model, check_view, &
        check_model, check_pointer
    implicit none
    class(IFloors), allocatable :: obj
    class(IFloors), pointer :: ptr
    type(Floors), target :: f
    type(Model) :: m
    real(8) :: p(5)

    obj = Floors()
    p = 1.d2
    p(:) = obj%pfloor(p(:))
    if (any(p /= 1.d2)) error stop 20
    p = [1.d2, -1.d0, 0.d0, 1.d-60, 2.d-50]
    p = obj%pfloor(p)
    if (any(p /= [1.d2, 1.d-50, 1.d-50, 1.d-50, 2.d-50])) error stop 21
    call check_view(make_floors(1d0, 2d0))
    allocate(m%source, source=make_floors(1d0, 2d0))
    call check_model(m)
    f = make_floors(1d0, 2d0)
    ptr => f
    p = [-1d0, 2d0, -3d0, 4d0, -5d0]
    call check_pointer(ptr, 5, p)
    if (any(p /= [1d0, 2d0, 1d0, 4d0, 1d0])) error stop 22
    print '(a)', 'traits_runtime_elemental_separate_01 ok'
end program traits_runtime_elemental_separate_01
