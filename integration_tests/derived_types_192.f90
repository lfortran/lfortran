module derived_types_192_mod
    use iso_c_binding, only: c_char
    implicit none

    type :: tseq
        sequence
        integer :: before
        character(len=3) :: c(4)
        integer :: after
    end type tseq

    type, bind(c) :: tbind
        character(kind=c_char) :: c(4)
    end type tbind

    type :: outer
        type(tseq) :: s
    end type outer

contains

    subroutine check_section(x, e1, e2)
        character(len=*), intent(in) :: x(:)
        character(len=*), intent(in) :: e1, e2
        if (size(x) /= 2) error stop "check_section: wrong size"
        if (x(1) /= e1) error stop "check_section: wrong first element"
        if (x(2) /= e2) error stop "check_section: wrong second element"
    end subroutine check_section

end module derived_types_192_mod

program derived_types_192
    use derived_types_192_mod
    implicit none

    type(tseq) :: sq
    type(tseq) :: sqs(3)
    type(tbind) :: bd
    type(outer) :: o
    character(len=3) :: c(2)
    character(len=1) :: c1(2)

    sq%before = 11
    sq%after = 22
    sq%c = ["aaa", "bbb", "ccc", "ddd"]

    ! The whole component and a single element already worked; keep them covered.
    if (size(sq%c) /= 4) error stop "whole component: wrong size"
    if (sq%c(1) // sq%c(2) // sq%c(3) // sq%c(4) /= "aaabbbcccddd") error stop "whole component"
    if (sq%c(2) /= "bbb") error stop "single element"

    ! Printing a section of a character array component of a SEQUENCE type.
    print *, sq%c(2:3)

    ! Assigning a section of a character array component.
    c = sq%c(2:3)
    if (c(1) /= "bbb") error stop "section assignment: first element"
    if (c(2) /= "ccc") error stop "section assignment: second element"

    ! A section that does not start at the first element.
    c = sq%c(3:4)
    if (c(1) /= "ccc") error stop "trailing section: first element"
    if (c(2) /= "ddd") error stop "trailing section: second element"

    ! Strided section.
    c = sq%c(1:4:2)
    if (c(1) /= "aaa") error stop "strided section: first element"
    if (c(2) /= "ccc") error stop "strided section: second element"

    ! Section passed as an actual argument to character(len=*) :: x(:).
    call check_section(sq%c(2:3), "bbb", "ccc")
    call check_section(sq%c(1:4:2), "aaa", "ccc")

    ! The neighbouring components must be untouched.
    if (sq%before /= 11) error stop "component before the section"
    if (sq%after /= 22) error stop "component after the section"

    ! Section of a component of an array of the SEQUENCE type.
    sqs(2)%before = 0
    sqs(2)%after = 0
    sqs(2)%c = ["eee", "fff", "ggg", "hhh"]
    c = sqs(2)%c(2:3)
    if (c(1) /= "fff") error stop "array element component section: first element"
    if (c(2) /= "ggg") error stop "array element component section: second element"
    call check_section(sqs(2)%c(2:3), "fff", "ggg")

    ! The same for a bind(c) type, whose character components are laid out inline too.
    bd%c = ["p", "q", "r", "s"]
    c1 = bd%c(2:3)
    if (c1(1) /= "q") error stop "bind(c) section: first element"
    if (c1(2) /= "r") error stop "bind(c) section: second element"
    call check_section(bd%c(2:3), "q", "r")

    ! Section of an inline character component reached through an outer type.
    o%s%before = 0
    o%s%after = 0
    o%s%c = ["iii", "jjj", "kkk", "lll"]
    c = o%s%c(2:3)
    if (c(1) /= "jjj") error stop "nested component section: first element"
    if (c(2) /= "kkk") error stop "nested component section: second element"
    call check_section(o%s%c(2:3), "jjj", "kkk")

    ! The same section as an ASSOCIATE selector.
    associate (z => sq%c(2:3))
        if (size(z) /= 2) error stop "associate section: wrong size"
        if (z(1) /= "bbb") error stop "associate section: first element"
        if (z(2) /= "ccc") error stop "associate section: second element"
    end associate

end program derived_types_192
