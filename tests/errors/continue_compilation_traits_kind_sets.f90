! Located diagnostics for kind wildcards in type sets. A wildcard stands for
! every supported kind of an integer, real or complex type. Each numbered case
! is an independent violation; all other code is valid.
module cc_traits_kind_sets_contracts
    implicit none
    abstract interface :: IReal
        real(*)
    end interface IReal

    abstract interface :: INumeric
        integer(*) | real(*) | complex(*)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum
contains
    function twice{IReal :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x + x
    end function twice
end module cc_traits_kind_sets_contracts

! E01: a logical wildcard is not a numeric type-set member.
module cc_traits_kind_sets_logical
    implicit none
    abstract interface :: IFlags
        logical(*)
    end interface IFlags
end module cc_traits_kind_sets_logical

! E02: the wildcard must be the kind type parameter.
module cc_traits_kind_sets_len
    implicit none
    abstract interface :: ICount
        integer(len=*)
    end interface ICount
end module cc_traits_kind_sets_len

! E03: a wildcard is the only type parameter of its member.
module cc_traits_kind_sets_two_kinds
    implicit none
    abstract interface :: IReal8
        real(*, 8)
    end interface IReal8
end module cc_traits_kind_sets_two_kinds

! E04: an explicit member still needs a supported kind.
module cc_traits_kind_sets_invalid_kind
    implicit none
    abstract interface :: IOdd
        integer(*) | integer(3)
    end interface IOdd
end module cc_traits_kind_sets_invalid_kind

! E05: complex members are not ordered, so the set has no operator(<).
module cc_traits_kind_sets_complex_order
    implicit none
contains
    function smaller{integer(*) | complex(*) :: T}(x, y) result(r)
        type(T), intent(in) :: x, y
        logical             :: r
        r = x < y
    end function smaller
end module cc_traits_kind_sets_complex_order

! E06: real(10), the C_LONG_DOUBLE kind, is not one of REAL_KINDS.
module cc_traits_kind_sets_long_double
    use cc_traits_kind_sets_contracts, only: twice
    implicit none
contains
    subroutine double_it(x)
        real(10), intent(inout) :: x
        x = twice(x)
    end subroutine double_it
end module cc_traits_kind_sets_long_double

! E07: a closed runtime slot cannot return a complex scalar yet.
module cc_traits_kind_sets_complex_slot
    use cc_traits_kind_sets_contracts, only: ISum
    implicit none
contains
    subroutine use_view(summer)
        class(ISum), intent(in) :: summer
    end subroutine use_view
end module cc_traits_kind_sets_complex_slot
