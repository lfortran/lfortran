! The same type argument checked with two conformances, each kept by the
! runtime view its generic implementation forms.
module traits_generic_requirement_07_blocks
    use traits_generic_requirement_04_contracts, only: Applying => IApply
    use traits_generic_requirement_04_types, only: Thing => Value
    implicit none

contains

    ! 62 + 1162 for Offset, 55 + 1155 for Relay.
    function both_scopes{Applying :: A}(alg) result(r)
        type(A), intent(in) :: alg
        integer :: r, first, second
        block
            use traits_generic_requirement_04_provider_a
            first = alg%apply(Thing())
        end block
        block
            use traits_generic_requirement_04_provider_b
            second = alg%apply{Thing}(Thing())
        end block
        r = first + second
    end function both_scopes

end module traits_generic_requirement_07_blocks
