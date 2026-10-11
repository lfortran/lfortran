! A module that instantiates, in its own procedure, generic definitions whose
! runtime views keep the recorded conformance. It uses neither provider.
module traits_generic_requirement_07_user
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_07_frozen, only: frozen_arg, make_offset, make_relay, &
        Offset, Relay
    implicit none

contains

    integer function instantiated()
        type(Value) :: v
        instantiated = frozen_arg(make_offset(), v) + frozen_arg(make_relay(), v)
    end function instantiated

end module traits_generic_requirement_07_user
