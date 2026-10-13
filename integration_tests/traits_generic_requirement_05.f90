! The client sees its own conformance of Value, from provider B. Calls checked
! in the generic definitions keep the conformances selected there, while a
! forwarded type argument takes the client's.
program traits_generic_requirement_05
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_04_provider_b
    use traits_generic_requirement_04_frozen, only: frozen, frozen_pair, forwarded, &
        relay, Offset, Combiner
    use traits_generic_requirement_04_blocks, only: both_scopes, through
    use traits_generic_requirement_05_user, only: instantiated
    implicit none
    type(Value) :: v

    if (v%value() /= 105) error stop 1
    if (frozen(Offset()) /= 12) error stop 2
    if (frozen_pair(Combiner()) /= 55) error stop 3
    if (forwarded(Offset(), v) /= 112) error stop 4
    if (both_scopes(Offset()) /= 124) error stop 5
    if (relay(Offset()) /= 12) error stop 6
    if (through(Offset(), v) /= 12) error stop 7
    if (instantiated() /= 24) error stop 8
    print '(a)', 'traits_generic_requirement_05 ok'
end program traits_generic_requirement_05
