! Trait guards in a consumer compiled with only the contracts (TD-11): the
! conformances declared with the provider's private types are found through
! their runtime conformance tables, a retroactive one only where visible.
program traits_runtime_conformance_query_separate_01
    use traits_runtime_conformance_query_separate_01_contracts
    use traits_runtime_conformance_query_separate_01_consumer
    use traits_runtime_conformance_query_separate_01_seeing
    implicit none
    class(*), allocatable :: any
    class(Base), allocatable :: base_object
    class(IValue), allocatable :: owner

    any = make_any(1, 3)
    if (classify_any(any) /= 103) error stop 1
    any = make_any(2, 3)
    if (classify_any(any) /= 153) error stop 2
    any = make_any(3, 3)
    if (classify_any(any) /= 7206) error stop 3
    any = make_any(0, 3)
    if (classify_any(any) /= -1 .or. seeing_label(any) /= 43) error stop 4
    any = make_any(3, 4)
    if (seeing_label(any) /= 7) error stop 5
    any = make_any(1, 4)
    if (seeing_label(any) /= -1) error stop 6

    base_object = make_base(3, 1)
    if (classify_base(base_object) /= 7 .or. base_object%n /= 6) error stop 7
    base_object = make_base(2, 1)
    if (classify_base(base_object) /= 51 .or. base_object%n /= 1) error stop 8

    owner = make_value(3, 10)
    if (classify_value(owner) /= 18) error stop 9
    owner = make_value(1, 10)
    if (classify_value(owner) /= -1) error stop 10
    print *, "trait guards: contract-only consumer and visible retroactive conformance"
end program
