! A generic interface named after the intrinsic `lgt`, with module
! procedures whose arguments mix a derived type and a character string,
! must resolve each call to the matching specific procedure, while the
! intrinsic `lgt` remains available for two character arguments (#3592).
module generic_name_13_mod
    implicit none

    type :: string_type
        sequence
        character(len=:), allocatable :: raw
    end type string_type

    interface lgt
        module procedure :: lgt_string_char
        module procedure :: lgt_char_string
    end interface lgt

contains

    elemental function lgt_string_char(lhs, rhs) result(is_lgt)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lgt
        is_lgt = lgt(lhs%raw, rhs)
    end function lgt_string_char

    elemental function lgt_char_string(lhs, rhs) result(is_lgt)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lgt
        is_lgt = lgt(lhs, rhs%raw)
    end function lgt_char_string

end module generic_name_13_mod

program generic_name_13
    use generic_name_13_mod, only: string_type, lgt
    implicit none

    type(string_type) :: string
    logical :: res

    string%raw = "bcd"
    res = lgt(string, "bcd") .eqv. lgt("bcd", "bcd")
    if (.not. res) error stop 1
    res = lgt("bcd", string) .eqv. lgt("bcd", "bcd")
    if (.not. res) error stop 2
    print *, res

    if (lgt(string, "bcd")) error stop 3
    if (.not. lgt(string, "abc")) error stop 4
    if (lgt(string, "cde")) error stop 5
    if (lgt("bcd", string)) error stop 6
    if (lgt("abc", string)) error stop 7
    if (.not. lgt("cde", string)) error stop 8
    if (.not. lgt("b", "a")) error stop 9
    if (lgt("a", "b")) error stop 10
end program generic_name_13
