program char4_string_intrinsics_02
    implicit none
    integer, parameter :: ucs4 = selected_char_kind("ISO_10646")
    ! U+4F60 and U+597D, two CJK ideographs outside the ASCII range
    character(kind=ucs4, len=1), parameter :: ni = char(int(z'4F60'), ucs4)
    character(kind=ucs4, len=1), parameter :: hao = char(int(z'597D'), ucs4)
    character(kind=ucs4, len=6) :: padded
    character(kind=ucs4, len=4) :: leading
    character(kind=ucs4, len=3) :: a, b
    character(kind=ucs4, len=:), allocatable :: joined
    integer :: code

    ! Give every variable a value before it is used: the kind inquiries below
    ! do not depend on the values, but their arguments are still evaluated.
    padded = ni // hao // ucs4_"    "
    leading = ucs4_"  " // ni // hao
    a = ni // hao // ucs4_"!"
    b = ucs4_"abc"

    ! Every character intrinsic must return a result of the same character
    ! kind as its argument, not the default kind.
    if (kind(trim(padded)) /= 4) error stop 1
    if (kind(adjustl(leading)) /= 4) error stop 2
    if (kind(adjustr(padded)) /= 4) error stop 3
    if (kind(repeat(a, 2)) /= 4) error stop 4
    if (kind(a // b) /= 4) error stop 5
    if (kind(new_line(a)) /= 4) error stop 6
    if (kind(char(65, ucs4)) /= 4) error stop 7
    if (kind(achar(65, ucs4)) /= 4) error stop 8

    ! char() with a non-constant code point and a constant kind must still
    ! build a four byte character.
    code = int(z'4F60')
    if (ichar(char(code, ucs4), kind=4) /= int(z'4F60')) error stop 9
    if (ichar(achar(code, ucs4), kind=4) /= int(z'4F60')) error stop 10

    ! trim/adjustl/adjustr must count and compare whole characters, so a
    ! trailing blank of kind 4 is one character wide, not one byte.
    if (len_trim(padded) /= 2) error stop 11
    if (len(trim(padded)) /= 2) error stop 12
    if (trim(padded) /= ni // hao) error stop 13

    if (len_trim(adjustl(leading)) /= 2) error stop 14
    if (adjustl(leading) /= ni // hao // ucs4_"  ") error stop 15
    if (adjustr(ni // hao // ucs4_"  ") /= ucs4_"  " // ni // hao) error stop 16

    ! Concatenation of two run time values keeps the kind and the length.
    joined = a // b
    if (len(joined) /= 6) error stop 17
    if (kind(joined) /= 4) error stop 18
    if (ichar(joined(1:1), kind=4) /= int(z'4F60')) error stop 19
    if (ichar(joined(2:2), kind=4) /= int(z'597D')) error stop 20
    if (joined(4:6) /= ucs4_"abc") error stop 21

    if (repeat(ni, 3) /= ni // ni // ni) error stop 22
    if (len(repeat(ni, 3)) /= 3) error stop 23

    ! index/scan/verify report character positions, not byte offsets.
    if (index(padded, hao) /= 2) error stop 24
    if (scan(padded, hao) /= 2) error stop 25
    if (verify(padded, ni // hao // ucs4_" ") /= 0) error stop 26
    if (index(padded, ni // hao) /= 1) error stop 27

    call check_folded()

    print *, "ok"

contains

    ! The same properties, but computed at compile time: constant folding must
    ! use the same character counts as the generated code.
    subroutine check_folded()
        character(kind=ucs4, len=6), parameter :: cpadded = ni // hao // ucs4_"    "
        character(kind=ucs4, len=4), parameter :: clead = ucs4_"  " // ni // hao
        if (len(cpadded) /= 6) error stop 31
        if (len_trim(cpadded) /= 2) error stop 32
        if (len(trim(cpadded)) /= 2) error stop 33
        if (trim(cpadded) /= ni // hao) error stop 34
        if (ichar(ni, kind=4) /= int(z'4F60')) error stop 35
        if (len(ni // hao) /= 2) error stop 36
        if (len_trim(adjustl(clead)) /= 2) error stop 37
        if (adjustl(clead) /= ni // hao // ucs4_"  ") error stop 38
        if (adjustr(cpadded) /= ucs4_"    " // ni // hao) error stop 39
        if (len(repeat(ni, 3)) /= 3) error stop 40
        if (index(cpadded, hao) /= 2) error stop 41
        if (scan(cpadded, hao) /= 2) error stop 42
        if (verify(cpadded, ni // hao // ucs4_" ") /= 0) error stop 43
    end subroutine

end program
