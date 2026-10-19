! The contract, a contract-only consumer, the provider type Euler and its
! extension TracedEuler are compiled separately, in that order. Views that the
! provider forms from class(Euler) objects, and views, pointers and owners
! that this program forms, dispatch to TracedEuler's overrides and keep its
! dynamic type.
program traits_runtime_polymorphic_source_separate_01
    use traits_runtime_polymorphic_source_separate_01_contracts_m, only: IStep
    use traits_runtime_polymorphic_source_separate_01_consumer_m, only: advance, peek, &
        advance_pointer
    use traits_runtime_polymorphic_source_separate_01_provider_m, only: Euler, drive, &
        count_of
    use traits_runtime_polymorphic_source_separate_01_extension_m, only: TracedEuler
    implicit none
    class(Euler), allocatable, target :: c
    type(Euler) :: e
    class(IStep), pointer :: p
    class(IStep), allocatable :: o
    real(8) :: y

    allocate(TracedEuler :: c)
    select type (x => c)
    type is (TracedEuler)
        allocate(x%log(0))
    end select
    y = 0
    call c%run(y)
    if (abs(y - 5) > 1d-12 .or. c%nsteps /= 1) error stop 1
    y = 0
    call drive(c, y)
    if (abs(y - 5) > 1d-12 .or. count_of(c) /= 2002) error stop 2
    y = 0
    call drive(e, y)
    if (abs(y - 0.5d0) > 1d-12 .or. count_of(e) /= 1) error stop 3

    y = 0
    call advance(c, y)
    if (abs(y - 5) > 1d-12 .or. peek(c) /= 3003) error stop 4

    p => c
    y = 0
    call advance_pointer(p, y)
    if (abs(y - 5) > 1d-12 .or. c%nsteps /= 4) error stop 5
    select type (x => p)
    type is (TracedEuler)
        if (any(x%log /= [1, 2, 3, 4])) error stop 6
    class default
        error stop 7
    end select

    allocate(o, source=c)
    call advance(o, y)
    if (peek(o) /= 5005 .or. peek(c) /= 4004) error stop 8
    select type (x => o)
    type is (TracedEuler)
        if (any(x%log /= [1, 2, 3, 4, 5])) error stop 9
    class default
        error stop 10
    end select
    deallocate(o)
    print *, "PASS"
end program
