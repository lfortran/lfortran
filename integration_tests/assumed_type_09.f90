! A `type(*) buf(*)` dummy of a non-bind(c) procedure carries no type
! information: forwarding it to a bind(c) `type(*) buf(*)` dummy passes the
! address of the caller's data.
module assumed_type_09_mod
    use iso_c_binding
    implicit none
    interface
        function first(t, buf) result(e) bind(c, name='assumed_type_09_first')
            import :: c_int
            integer(c_int), value :: t
            type(*), intent(in) :: buf(*)
            integer(c_int) :: e
        end function
        function chars(t, buf, expected, n) result(e) bind(c, name='assumed_type_09_chars')
            import :: c_int, c_char
            integer(c_int), value :: t
            type(*), intent(in) :: buf(*)
            character(kind=c_char), intent(in) :: expected(*)
            integer(c_int), value :: n
            integer(c_int) :: e
        end function
    end interface
contains
    function wr_f(t, buf) result(e)
        integer, intent(in) :: t
        type(*), intent(in) :: buf(*)
        integer(c_int) :: e
        e = first(t, buf)
    end function

    function wr_chars(t, buf, expected, n) result(e)
        integer, intent(in) :: t, n
        type(*), intent(in) :: buf(*)
        character(len=*), intent(in) :: expected
        integer(c_int) :: e
        e = chars(t, buf, expected, n)
    end function

    function via_shape(a) result(e)
        integer(c_int64_t), intent(in) :: a(:)
        integer(c_int) :: e
        e = wr_f(1, a)
    end function
end module

program assumed_type_09
    use assumed_type_09_mod
    implicit none
    integer(c_int64_t) :: n, arr(2)
    character(len=3) :: s, sa(2)

    n = 4
    arr = [7, 8]
    if (wr_f(1, arr) /= 7) error stop 1
    if (wr_f(1, [n]) /= 4) error stop 2
    if (wr_f(1, [arr]) /= 7) error stop 3
    if (via_shape(arr) /= 7) error stop 4
    if (via_shape(arr(2:2)) /= 8) error stop 5
    if (wr_f(1, arr(2:)) /= 8) error stop 6

    s = 'abc'
    if (wr_chars(1, s, 'abc', 3) /= 0) error stop 7
    sa = ['abc', 'def']
    if (wr_chars(1, sa, 'abcdef', 6) /= 0) error stop 8
    print *, "ok"
end program
