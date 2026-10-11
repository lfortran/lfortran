! Generic definitions under renamed imports whose calls see a conformance of
! Value only through a USE local to a BLOCK or to the procedure. Each call
! keeps the conformance visible where it was checked, also when a separately
! compiled client instantiates it.
module traits_generic_requirement_04_blocks
    use traits_generic_requirement_04_contracts, only: Applying => IApply
    use traits_generic_requirement_04_types, only: Thing => Value
    use traits_generic_requirement_04_frozen, only: frozen_arg
    implicit none

contains

    ! The same type argument checked with two conformances: two
    ! specializations, 12 + 112.
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

    function local_use{Applying :: A}(alg) result(r)
        use traits_generic_requirement_04_provider_b
        type(A), intent(in) :: alg
        integer :: r
        r = alg%apply(Thing())
    end function local_use

    ! Forwarding to frozen_arg republishes its specialization here with the
    ! conformance its definition selected, which this module cannot see.
    function through{Applying :: B}(alg, v) result(r)
        type(B), intent(in) :: alg
        type(Thing), intent(in) :: v
        integer :: r
        r = frozen_arg(alg, v)
    end function through

end module traits_generic_requirement_04_blocks
