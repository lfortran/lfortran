! Polymorphic results of trait messages cross separately compiled modules: the
! consumer is compiled without the provider of the conforming type, which
! declares a dynamic type of the results.
program traits_runtime_polymorphic_result_separate_01
    use traits_runtime_polymorphic_result_separate_01_contracts_m, only: ILookup, Point
    use traits_runtime_polymorphic_result_separate_01_provider_m, only: Table, Point3
    use traits_runtime_polymorphic_result_separate_01_consumer_m, only: Model, label, &
        check_view, check_model, check_pointer
    implicit none
    type(Table), target :: t
    type(Model) :: m
    class(ILookup), pointer :: p
    class(ILookup), allocatable :: owner
    class(*), allocatable :: r

    t = Table(4)
    call check_view(t, 4)
    call check_view(Table(1), 1)
    allocate(m%source, source=Table(3))
    call check_model(m, 3)
    p => t
    call check_pointer(p, 4)
    owner = Table(2)
    call check_view(owner, 2)
    r = owner%lookup("xyz")
    select type (r)
    type is (Point3)
        if (r%x /= 2 .or. r%y /= 3 .or. r%z /= -1) error stop 20
    class default
        error stop 21
    end select
    r = label(owner)
    select type (r)
    type is (character(*))
        if (r /= "tt") error stop 22
    class default
        error stop 23
    end select
    print '(a)', 'traits_runtime_polymorphic_result_separate_01 ok'
end program traits_runtime_polymorphic_result_separate_01
