! Located diagnostics for a module of the program with the name under which
! LFortran provides the intrinsic module generics: it is an ordinary module,
! which never stands in for the intrinsic module.
! Each numbered case is an independent violation; all other code is valid.
module lfortran_intrinsic_generics
    implicit none
    abstract interface :: INumeric
        integer
    end interface INumeric
end module lfortran_intrinsic_generics

! The module of the program, used by its name.
module cc_traits_kind_sets_own_name
    use lfortran_intrinsic_generics, only: INumeric
    implicit none
end module cc_traits_kind_sets_own_name

! E01: an INTRINSIC use accesses the intrinsic module only.
module cc_traits_kind_sets_intrinsic_name
    use, intrinsic :: generics, only: INumeric
    implicit none
end module cc_traits_kind_sets_intrinsic_name

! E02: so does a use without a module nature when the program has no module
! named generics.
module cc_traits_kind_sets_plain_name
    use generics, only: INumeric
    implicit none
end module cc_traits_kind_sets_plain_name

! E03: and an INTRINSIC use in a BLOCK.
subroutine cc_traits_kind_sets_block_name()
    implicit none
    block
        use, intrinsic :: generics, only: INumeric
    end block
end subroutine cc_traits_kind_sets_block_name
