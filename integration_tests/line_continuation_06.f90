program line_continuation_06
    ! `&!` inside a character literal must be kept verbatim: `!` never
    ! starts a comment inside a string, and `&` is only a continuation
    ! when it is the last non-blank character on the line.
    implicit none
    character(len=*), parameter :: expected = "x" // achar(38) // "!y"
    character(len=*), parameter :: expected_amp = "x" // achar(38) // "y"
    ! "&!" inside a plain single-line literal
    character(len=*), parameter :: a = "x&!y"
    ! "&!" inside a literal continued inside the quotes
    character(len=*), parameter :: b = "x&!&
&y"
    ! "&!" at the start of the continuation line
    character(len=*), parameter :: c = "x&
&&!y"
    ! "&" alone across a continuation (control: no "!")
    character(len=*), parameter :: d = "x&&
&y"
    ! "&" followed by blanks and "!" inside a literal
    character(len=*), parameter :: e = "x& !y"
    ! single-quoted literal
    character(len=*), parameter :: f = 'x&!y'
    character(len=10) :: g

    print *, len(a), "|", a, "|"
    if (len(a) /= 4) error stop
    if (a /= expected) error stop

    print *, len(b), "|", b, "|"
    if (len(b) /= 4) error stop
    if (b /= expected) error stop

    print *, len(c), "|", c, "|"
    if (len(c) /= 4) error stop
    if (c /= expected) error stop

    print *, len(d), "|", d, "|"
    if (len(d) /= 3) error stop
    if (d /= expected_amp) error stop

    print *, len(e), "|", e, "|"
    if (len(e) /= 5) error stop
    if (e /= "x" // achar(38) // " !y") error stop

    print *, len(f), "|", f, "|"
    if (len(f) /= 4) error stop
    if (f /= expected) error stop

    g = "&!"
    print *, "|", trim(g), "|"
    if (len_trim(g) /= 2) error stop
    if (trim(g) /= achar(38) // "!") error stop

    ! outside a string, `&!` is a continuation with a trailing comment
    g = "ab" // &! comment
        "cd"
    if (trim(g) /= "abcd") error stop
end program line_continuation_06
