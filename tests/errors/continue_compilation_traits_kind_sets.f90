! Located diagnostics for kind wildcards in type sets and the intrinsic module
! generics; a wildcard stands for every supported kind of its numeric type.
! Each numbered case is an independent violation; all other code is valid.
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

! E01: logical members have no arithmetic.
module cc_traits_kind_sets_logical
    implicit none
contains
    function twice{logical(*) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x + x
    end function twice
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

! E07: a character length wildcard would need a closed runtime slot for every
! length.
module cc_traits_kind_sets_character_slot
    implicit none
    abstract interface :: ICode
        character(len=3) | character(*)
    end interface ICode

    abstract interface :: IPick
        function pick{ICode :: T}(x) result(r)
            type(T), intent(in) :: x(:)
            type(T)             :: r
        end function pick
    end interface IPick
contains
    subroutine use_view(picker)
        class(IPick), intent(in) :: picker
    end subroutine use_view
end module cc_traits_kind_sets_character_slot

! E08: the intrinsic module generics provides INumeric only.
module cc_traits_kind_sets_any_type
    use, intrinsic :: generics, only: IAnyType
    implicit none
end module cc_traits_kind_sets_any_type

! The program's own module named generics, an ordinary module.
module generics
    implicit none
    abstract interface :: INumeric
        integer
    end interface INumeric
contains
    integer function origin()
        origin = 1
    end function origin
end module generics

! E09: an intrinsic USE never reaches the program's own module generics.
module cc_traits_kind_sets_intrinsic_origin
    use, intrinsic :: generics, only: origin
    implicit none
end module cc_traits_kind_sets_intrinsic_origin

! E10: a USE without module nature reaches the program's own module, whose
! INumeric admits only the default integer.
module cc_traits_kind_sets_own_numeric
    use generics, only: INumeric
    implicit none
contains
    function total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function total

    subroutine sum_reals(x)
        real(8), intent(in) :: x(:)
        print *, total(x)
    end subroutine sum_reals
end module cc_traits_kind_sets_own_numeric

! Two uses of the program's own module generics, with and without a nature.
module cc_traits_kind_sets_two_uses
    use generics, only: INumeric
    use, non_intrinsic :: generics, only: origin
    implicit none
end module cc_traits_kind_sets_two_uses

! E11: one scoping unit cannot access both the program's module generics and
! the intrinsic module generics.
module cc_traits_kind_sets_both_natures
    use generics, only: Mine => INumeric
    use, intrinsic :: generics, only: Theirs => INumeric
    implicit none
end module cc_traits_kind_sets_both_natures

! E12: nor can a BLOCK.
subroutine cc_traits_kind_sets_block_natures()
    implicit none
    block
        use, non_intrinsic :: generics, only: Mine => INumeric
        use, intrinsic :: generics, only: Theirs => INumeric
    end block
end subroutine cc_traits_kind_sets_block_natures

! E13: the relational operators of standard Fortran do not apply to logical
! values, so logical members have no operator(==).
module cc_traits_kind_sets_logical_equal
    implicit none
contains
    function same{logical | logical(1) :: T}(x, y) result(r)
        type(T), intent(in) :: x, y
        logical             :: r
        r = x == y
    end function same
end module cc_traits_kind_sets_logical_equal

! E14: integer members have no operator(.and.).
module cc_traits_kind_sets_mixed_and
    implicit none
contains
    function both{integer | logical :: T}(x, y) result(r)
        type(T), intent(in) :: x, y
        type(T)             :: r
        r = x .and. y
    end function both
end module cc_traits_kind_sets_mixed_and

! E15: an integer is not a valid source of the intrinsic LOGICAL conversion.
module cc_traits_kind_sets_logical_conversion
    implicit none
contains
    function zero{logical(*) :: T}() result(r)
        type(T) :: r
        r = T(0)
    end function zero
end module cc_traits_kind_sets_logical_conversion

! E16: concatenating members of different lengths has a different result type
! for each member.
module cc_traits_kind_sets_concat_lengths
    implicit none
contains
    function joined{character(len=3) | character(len=5) :: T}(x, y) result(r)
        type(T), intent(in) :: x, y
        character(len=10)   :: r
        r = x // y
    end function joined
end module cc_traits_kind_sets_concat_lengths

! E17: the length of a character member is part of the member.
module cc_traits_kind_sets_character_length
    implicit none
contains
    function first{character(len=3) | character(len=5) :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        r = x(1)
    end function first

    subroutine use_first(words)
        character(len=4), intent(in) :: words(:)
        print *, first(words)
    end subroutine use_first
end module cc_traits_kind_sets_character_length

! E18: a condition must be logical for every member.
module cc_traits_kind_sets_mixed_condition
    implicit none
contains
    function count_true{integer | logical :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n, i
        n = 0
        do i = 1, size(x)
            if (x(i)) n = n + 1
        end do
    end function count_true
end module cc_traits_kind_sets_mixed_condition

! E19: character(kind=*) stands for CHARACTER_KINDS, which does not include
! kind 4 (ISO_10646).
module cc_traits_kind_sets_character_kind
    implicit none
contains
    function first{character(kind=*) :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        r = x(1)
    end function first

    subroutine use_first(letters)
        character(kind=4), intent(in) :: letters(:)
        character(kind=4)             :: letter
        letter = first(letters)
    end subroutine use_first
end module cc_traits_kind_sets_character_kind

! E20: a length wildcard admits each constant length as its own type
! argument, so an actual of assumed length is not one.
module cc_traits_kind_sets_assumed_length
    implicit none
contains
    function first{character(*) :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        r = x(1)
    end function first

    subroutine use_first(words)
        character(len=*), intent(in) :: words(:)
        print *, first(words)
    end subroutine use_first
end module cc_traits_kind_sets_assumed_length

! E21: the length of a concatenation of length wildcard values depends on the
! values.
module cc_traits_kind_sets_wildcard_concat
    implicit none
contains
    function joined_length{character(len=*) :: T}(x, y) result(n)
        type(T), intent(in) :: x, y
        integer             :: n
        n = len(x // y)
    end function joined_length
end module cc_traits_kind_sets_wildcard_concat

! E22: a character member of deferred length is not implemented yet.
module cc_traits_kind_sets_deferred_length
    implicit none
    abstract interface :: IDeferred
        character(len=:)
    end interface IDeferred
end module cc_traits_kind_sets_deferred_length
