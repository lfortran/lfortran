! The providers define same-named, unrelated Coeffs and Tally types; the
! consumer, compiled without them, updates the estimates of each provider
! through the coefficients of the other, which see them at their original
! addresses with their own implementations.
program traits_runtime_view_argument_separate_01
    use iso_c_binding, only: c_loc, c_associated
    use traits_runtime_view_argument_separate_01_estimates_m, only: IEstimates
    use traits_runtime_view_argument_separate_01_contracts_m, only: ICoeffs
    use traits_runtime_view_argument_separate_01_consumer_m, only: Model, blended, &
        update_through
    use traits_runtime_view_argument_separate_01_a_m, ACoeffs => Coeffs, ATally => Tally
    use traits_runtime_view_argument_separate_01_b_m, BCoeffs => Coeffs, BTally => Tally
    implicit none
    type(Model), target :: ab, ba
    type(ATally), target :: at
    type(BTally), target :: bt
    type(BCoeffs), target :: bc
    class(ICoeffs), pointer :: cp
    class(IEstimates), pointer :: ep

    ! A's coefficients step B's estimates three times per update.
    allocate(ab%coeffs, source=ACoeffs(stride=3))
    allocate(BTally :: ab%estimates)
    call ab%coefficients()
    call ab%coefficients()
    if (ab%estimates%total() /= 60) error stop 1
    select type (concrete => ab%estimates)
    type is (BTally)
        if (.not. c_associated(b_seen, c_loc(concrete%n))) error stop 2
    class default
        error stop 3
    end select
    ! B's coefficients step A's estimates once per update.
    allocate(BCoeffs :: ba%coeffs)
    allocate(ba%estimates, source=ATally(5))
    call ba%coefficients()
    if (ba%estimates%total() /= 6) error stop 4
    select type (concrete => ba%estimates)
    type is (ATally)
        if (.not. c_associated(a_seen, c_loc(concrete%n))) error stop 5
    class default
        error stop 6
    end select
    ! A read-only receiver view with a mutable argument, by keyword.
    if (blended(ab%coeffs, bt, 2) /= 22 .or. bt%n /= 10) error stop 7
    if (.not. c_associated(b_seen, c_loc(bt%n))) error stop 8
    if (blended(bc, at, 7) /= 7 .or. at%n /= 1) error stop 9
    if (.not. c_associated(a_seen, c_loc(at%n))) error stop 10
    ! Pointer targets through intent(in) pointers.
    cp => bc
    ep => at
    call update_through(cp, ep)
    if (bc%updates /= 1 .or. at%n /= 2) error stop 11
    ! A concrete receiver binds the message statically.
    call bc%update(ab%estimates)
    if (bc%updates /= 2 .or. ab%estimates%total() /= 70) error stop 12
    print '(a)', 'traits_runtime_view_argument_separate_01 ok'
end program traits_runtime_view_argument_separate_01
