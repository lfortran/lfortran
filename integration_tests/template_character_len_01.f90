! Character lengths in a template given by a deferred constant, an
! expression of it, another dummy argument of the procedure, or a local
! named constant of the procedure. They refer to the instantiation's
! entities, also in the length of a concatenation. The value of a named
! constant of such a length is padded or truncated to it.

module template_character_len_01_m
    implicit none

    template tm {n}
        deferred integer, parameter :: n

        type :: w
            character(len=n) :: c
            character(len=n+1) :: d
        end type
    contains
        integer function f(m, s) result(r)
            integer, intent(in) :: m
            character(len=m), intent(in) :: s
            integer, parameter :: k = n*2
            character(len=k) :: t
            character(len=n) :: u
            t = s
            u = s
            r = len(s) + len(t) + len(u)
            if (t /= s) error stop
            if (len(t // u) /= k + n) error stop
            if (len(u // 'q') /= n + 1) error stop
            if (t // u // 'q' /= 'xy    xy q') error stop
        end function

        subroutine pc()
            character(len=n), parameter :: c = 'abcdef'
            character(len=n), parameter :: d = 'ab'
            character(len=n), parameter :: a(2) = ['abcdef', 'ghijkl']
            character(len=n), parameter :: b(2) = ['x', 'y']
            print *, c == 'abc', d == 'ab ', len(c)
            if (c /= 'abc') error stop
            if (len(c) /= n) error stop
            if (d /= 'ab ') error stop
            if (a(2) /= 'ghi') error stop
            if (b(1) /= 'x  ') error stop
            if (len(d // 'x') /= n + 1) error stop
            if (d // 'x' /= 'ab x') error stop
            if (a(2) // '|' /= 'ghi|') error stop
            if (len(a(1) // b(2)) /= 2*n) error stop
        end subroutine
    end template

    requirement rq {t}
        deferred type :: t
    end requirement

    template tt {t}
        require :: rq {t}
    contains
        subroutine cs(n, x, s, k)
            integer, intent(in) :: n
            type(t), intent(inout) :: x
            character(len=n), intent(in) :: s
            integer, intent(out) :: k
            k = len(s) + size([x, x])
        end subroutine
    end template
end module

program template_character_len_01
    use template_character_len_01_m
    implicit none
    instantiate tm {3}, only: w3 => w, f3 => f, pc3 => pc
    instantiate tt {integer}, only: cs_i => cs
    instantiate tt {real}, only: cs_r => cs
    type(w3) :: s
    integer :: k, v
    real :: r

    s%c = "abcdef"
    s%d = "abcdef"
    print *, len(s%c), len(s%d), s%c, s%d
    if (len(s%c) /= 3) error stop
    if (len(s%d) /= 4) error stop
    if (s%c /= "abc") error stop
    if (s%d /= "abcd") error stop

    k = f3(2, "xy")
    print *, k
    if (k /= 11) error stop
    call pc3()

    v = 1
    call cs_i(3, v, "abc", k)
    if (k /= 5) error stop
    r = 1
    call cs_r(2, r, "ab", k)
    if (k /= 4) error stop
end program
