! The client sees its own conformance of Value, from provider B. The runtime
! views of calls checked in the generic definitions keep the conformances
! selected there, while a forwarded type argument takes the client's for both
! its static and its runtime uses.
program traits_generic_requirement_08
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_04_provider_b
    use traits_generic_requirement_07_frozen, only: frozen, forwarded, make_offset, &
        make_relay, Offset, Relay
    use traits_generic_requirement_07_blocks, only: both_scopes
    implicit none
    type(Value) :: v

    if (v%value() /= 105) error stop 1
    if (frozen(make_offset()) /= 62) error stop 2
    if (frozen(make_relay()) /= 55) error stop 3
    if (forwarded(make_offset(), v) /= 1162) error stop 4
    if (forwarded(make_relay(), v) /= 1155) error stop 5
    if (both_scopes(make_offset()) /= 1224) error stop 6
    if (both_scopes(make_relay()) /= 1210) error stop 7
    print '(a)', 'traits_generic_requirement_08 ok'
end program traits_generic_requirement_08
