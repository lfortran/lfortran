! Array results of trait messages cross separately compiled modules: the
! consumer is compiled without the provider of the conforming type.
program traits_runtime_array_result_separate_01
    use traits_runtime_array_result_separate_01_contracts_m, only: ISamples
    use traits_runtime_array_result_separate_01_provider_m, only: Ramp
    use traits_runtime_array_result_separate_01_consumer_m, only: Model, total, &
        doubled, window_sum, check_model, check_pointer
    implicit none
    type(Ramp), target :: r
    type(Model) :: m
    class(ISamples), pointer :: p
    class(ISamples), allocatable :: owner
    real(8), allocatable :: y(:)

    r = Ramp(4, 0.5d0)
    if (total(r) /= 5) error stop 10
    y = doubled(r)
    if (size(y) /= 4 .or. any(y /= [1, 2, 3, 4])) error stop 11
    if (window_sum(r, 3) /= 5 + 6 + 7 + 4) error stop 12
    allocate(m%source, source=Ramp(3, 2d0))
    call check_model(m, [2d0, 4d0, 6d0])
    p => r
    call check_pointer(p, 4)
    owner = Ramp(2, 3d0)
    y = owner%samples()
    if (any(y /= [3, 6])) error stop 13
    if (any(owner%scaled([1d0, -1d0]) /= [3, -3])) error stop 14
    print '(a)', 'traits_runtime_array_result_separate_01 ok'
end program traits_runtime_array_result_separate_01
