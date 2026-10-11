! A module that instantiates generic definitions with recorded conformances in
! its own procedure. Its separately compiled module file keeps them, though it
! uses neither provider.
module traits_generic_requirement_05_user
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_04_frozen, only: frozen_arg, Offset
    use traits_generic_requirement_04_blocks, only: through
    implicit none

contains

    integer function instantiated()
        type(Value) :: v
        instantiated = frozen_arg(Offset(), v) + through(Offset(), v)
    end function instantiated

end module traits_generic_requirement_05_user
