module string_arg_hidden_length_03_m
    ! Character dummies whose declared length depends on the length or the
    ! value of a dummy that comes later in the argument list (each entity is
    ! declared before it is used in a specification expression).
    implicit none

    integer :: modlen = 4

contains

    pure integer function nlen(s)
        character(len=*), intent(in) :: s
        nlen = len_trim(s) + 1
    end function

    ! Scalars: b depends on a, which depends on c.
    subroutine scalars(b, a, c)
        character(len=*), intent(in) :: c
        character(len=len(c)+1), intent(in) :: a
        character(len=len(a)*2), intent(in) :: b
        if (len(c) /= 3) error stop 1
        if (len(a) /= 4) error stop 2
        if (len(b) /= 8) error stop 3
        if (c /= 'xyz') error stop 4
        if (a /= 'ABCD') error stop 5
        if (b /= '01234567') error stop 6
    end subroutine

    ! An array whose element length depends on a scalar that depends on an
    ! integer dummy.
    subroutine array_elements(b, a, n)
        integer, intent(in) :: n
        character(len=n+1), intent(in) :: a
        character(len=len(a)+2), intent(in) :: b(2)
        if (len(a) /= 4) error stop 11
        if (len(b) /= 6) error stop 12
        if (a /= 'ABCD') error stop 13
        if (b(1) /= 'abcdef') error stop 14
        if (b(2) /= 'ghijkl') error stop 15
    end subroutine

    ! A chain of four, in reverse order, with an assumed-size array in it,
    ! and a length that depends on the element length of an array dummy.
    subroutine chain(e, d, c, b, a, w)
        character(len=*), intent(in) :: w(*)
        character(len=*), intent(in) :: a
        character(len=len(a)+1), intent(in) :: b
        character(len=len(b)*2), intent(in) :: c(*)
        character(len=len(c)-len(a)), intent(in) :: d
        character(len=len(w(1))+len(d)), intent(in) :: e
        if (len(a) /= 2) error stop 21
        if (len(b) /= 3) error stop 22
        if (len(c) /= 6) error stop 23
        if (len(d) /= 4) error stop 24
        if (len(w) /= 5) error stop 25
        if (len(e) /= 9) error stop 26
        if (c(1) /= 'abcdef' .or. c(2) /= 'ghijkl') error stop 27
        if (d /= 'wxyz') error stop 28
        if (e /= '123456789') error stop 29
        if (w(2) /= 'VWXYZ') error stop 30
    end subroutine

    ! Lengths from VALUE and INTENT(IN) integer dummies and from the lengths
    ! of other dummies; the result length depends on them too.
    function combine(b, a, k, m) result(r)
        integer, value :: k
        integer, intent(in) :: m
        character(len=m), intent(in) :: a
        character(len=k+len(a)), intent(in) :: b
        character(len=len(a)+len(b)) :: r
        r = a // b
    end function

    ! An intent(out) dummy whose length depends on a later dummy.
    subroutine fill(out, src)
        character(len=*), intent(in) :: src
        character(len=len(src)+1), intent(out) :: out
        out = src // '!'
    end subroutine

    ! Lengths from a module variable, from len_trim and index of another
    ! dummy, from size of an assumed-shape array, and from size and len of
    ! an explicit-shape character array whose length depends on index.
    subroutine intrinsic_lengths(a, b, c, d, e, arr, w, n)
        integer, intent(in) :: arr(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: c
        character(len=index(c, 'x')), intent(in) :: w(n)
        character(len=modlen+1), intent(in) :: a
        character(len=len_trim(c)), intent(in) :: b
        character(len=size(arr)), intent(in) :: d
        character(len=size(w)+len(w)), intent(in) :: e
        if (len(a) /= 5) error stop 61
        if (len(b) /= 3) error stop 62
        if (len(c) /= 6) error stop 63
        if (len(d) /= 3) error stop 64
        if (len(e) /= 5) error stop 65
        if (len(w) /= 3) error stop 66
        if (a /= 'abcde') error stop 67
        if (b /= 'abc') error stop 68
        if (d /= 'abc') error stop 69
        if (e /= 'abcde') error stop 70
        if (w(1) /= 'zzz' .or. w(2) /= 'zyy') error stop 71
    end subroutine

    ! An internal procedure whose dummy length depends on the host's
    ! assumed-length dummy.
    subroutine host(h)
        character(len=*), intent(in) :: h
        call inner('abcdefgh')
    contains
        subroutine inner(z)
            character(len=len(h)+1), intent(in) :: z
            if (len(z) /= 4) error stop 81
            if (z /= 'abcd') error stop 82
        end subroutine
    end subroutine

    ! Lengths from a pure function of an assumed-length dummy, of a
    ! substring of it, and of another dummy, for a scalar dummy, an
    ! assumed-size array dummy and a local.
    subroutine function_lengths(a, c, w)
        character(len=*), intent(in) :: c
        character(len=nlen(c)), intent(in) :: a
        character(len=nlen(c(2:))), intent(in) :: w(*)
        character(len=nlen(a)) :: loc
        loc = a
        if (len(a) /= 4) error stop 91
        if (len(w(1)) /= 3) error stop 92
        if (len(loc) /= 5) error stop 93
        if (a /= '0123') error stop 94
        if (w(1) /= 'abc' .or. w(2) /= 'def') error stop 95
        if (loc /= '0123 ') error stop 96
    end subroutine

end module

program string_arg_hidden_length_03
    use string_arg_hidden_length_03_m
    implicit none
    character(len=6) :: w(2) = ['abcdef', 'ghijkl']
    character(len=5) :: v(2) = ['ABCDE', 'VWXYZ']
    character(len=10) :: r
    character(len=8) :: o
    character(len=10) :: big = 'abcdefghij'
    character(len=4) :: ww(2) = ['zzzz', 'yyyy']
    integer :: iarr(3) = [1, 2, 3]

    call scalars('0123456789abcdef', 'ABCDEFGH', 'xyz')
    call array_elements(w, 'ABCD', 3)
    call chain('123456789xyz', 'wxyz--', w, 'PQR', 'ab', v)

    r = combine('1234567', 'abc', 4, 3)
    if (len(combine('1234567', 'abc', 4, 3)) /= 10) error stop 41
    if (r /= 'abc1234567') error stop 42

    o = '--------'
    call fill(o, 'hello')
    if (o /= 'hello!--') error stop 51

    call intrinsic_lengths(big, big, 'abx   ', big, big, iarr, ww, 2)
    call host('xyz')
    call function_lengths('0123456789', 'xyz   ', w)

    print *, 'ok'
end program
