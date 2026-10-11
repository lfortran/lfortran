! Generic definitions select the conformance of a concrete method type
! argument where their calls are checked (review 01c6aed8). This client imports
! only the generics, the implementing types and Value itself, and sees no
! conformance of Value at all.
program traits_generic_requirement_04
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_04_frozen, only: frozen, frozen_explicit, &
        frozen_pair, frozen_arg, relay, Offset, Combiner
    use traits_generic_requirement_04_blocks, only: both_scopes, local_use, through
    implicit none
    type(Value) :: v

    if (frozen(Offset()) /= 12) error stop 1
    if (frozen_explicit(Offset()) /= 24) error stop 2
    if (frozen_pair(Combiner()) /= 55) error stop 3
    if (both_scopes(Offset()) /= 124) error stop 4
    if (local_use(Offset()) /= 112) error stop 5
    if (frozen_arg(Offset(), v) /= 12) error stop 6
    if (relay(Offset()) /= 12) error stop 7
    if (through(Offset(), v) /= 12) error stop 8
    print '(a)', 'traits_generic_requirement_04 ok'
end program traits_generic_requirement_04
