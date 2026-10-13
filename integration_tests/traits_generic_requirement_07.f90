! A generic implementation that passes a concrete type argument to a generic
! method of a runtime trait object forms the runtime view with the conformance
! selected where the generic definition checked the call (review 0399 R1). This
! client sees no conformance of Value.
program traits_generic_requirement_07
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_07_frozen, only: frozen, frozen_arg, make_offset, &
        make_relay, Offset, Relay
    use traits_generic_requirement_07_blocks, only: both_scopes
    use traits_generic_requirement_07_user, only: instantiated
    implicit none
    type(Value) :: v

    if (frozen(make_offset()) /= 62) error stop 1
    if (frozen(make_relay()) /= 55) error stop 2
    if (frozen_arg(make_offset(), v) /= 62) error stop 3
    if (both_scopes(make_offset()) /= 1224) error stop 4
    if (both_scopes(make_relay()) /= 1210) error stop 5
    if (instantiated() /= 117) error stop 6
    print '(a)', 'traits_generic_requirement_07 ok'
end program traits_generic_requirement_07
