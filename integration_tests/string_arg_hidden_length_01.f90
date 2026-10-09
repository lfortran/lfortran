module string_arg_hidden_length_01_m
    ! Character dummy arguments of every kind of procedure, passed as a data
    ! pointer with a hidden length.
    implicit none

    type :: base_t
    contains
        procedure :: describe => base_describe
    end type

    type, extends(base_t) :: child_t
    contains
        procedure :: describe => child_describe
    end type

    abstract interface
        function str_fn(s, n) result(r)
            character(len=*), intent(in) :: s
            integer, intent(in) :: n
            integer :: r
        end function
    end interface

    integer :: n_calls = 0

contains

    ! len=* and len=n scalars, a default `character`, intent(out)
    subroutine scalars(a, n, b, c, d)
        character(len=*), intent(in) :: a
        integer, intent(in) :: n
        character(len=n), intent(in) :: b
        character, intent(in) :: c
        character(len=5), intent(out) :: d
        if (len(a) /= 7) error stop 1
        if (a /= 'abcdefg') error stop 2
        if (len(b) /= n) error stop 3
        if (b /= 'xyz') error stop 4
        if (len(c) /= 1) error stop 5
        if (c /= 'Q') error stop 6
        if (len(d) /= 5) error stop 7
        d = a(1:2) // b
    end subroutine

    ! An explicit-shape and an assumed-size array: element length
    subroutine arrays(x, n, y, total)
        integer, intent(in) :: n
        character(len=*), intent(inout) :: x(n)
        character(len=*), intent(in) :: y(*)
        character(len=*), intent(out) :: total
        integer :: i
        if (len(x) /= 3) error stop 10
        if (len(y) /= 2) error stop 11
        total = ''
        do i = 1, n
            total = trim(total) // x(i) // y(i)
        end do
        x(n) = 'zzz'
    end subroutine

    ! A declared element length different from the actual's: the dummy
    ! array is the actual's characters regrouped (sequence association).
    subroutine regroup(y)
        character(len=3), intent(in) :: y(2)
        if (y(1) /= 'abc') error stop 12
        if (y(2) /= 'def') error stop 13
    end subroutine

    subroutine opt(a, b)
        character(len=*), intent(in) :: a
        character(len=*), intent(in), optional :: b
        if (a /= 'first') error stop 20
        if (present(b)) then
            if (len(b) /= 6) error stop 21
            if (b(1:5) /= 'secon') error stop 22
        end if
    end subroutine

    elemental function first_code(s) result(r)
        character(len=*), intent(in) :: s
        integer :: r
        r = iachar(s(1:1)) + len(s)
    end function

    recursive function count_down(s) result(r)
        character(len=*), intent(in) :: s
        integer :: r
        if (len(s) == 0) then
            r = 0
        else
            r = 1 + count_down(s(2:))
        end if
    end function

    ! An allocatable and a pointer character dummy keep their descriptor
    subroutine deferred(a, p, s)
        character(len=:), allocatable, intent(inout) :: a
        character(len=:), pointer, intent(in) :: p
        character(len=*), intent(in) :: s
        a = a // s // p
    end subroutine

    ! An assumed-shape array keeps its descriptor
    integer function assumed_shape(x) result(r)
        character(len=*), intent(in) :: x(:)
        r = size(x) * 10 + len(x)
    end function

    integer function by_value(c, s) result(r)
        character, value :: c
        character(len=*), intent(in) :: s
        r = iachar(c) + len(s)
    end function

    function counted(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s) + 1) :: r
        n_calls = n_calls + 1
        r = s // '!'
    end function

    ! An internal procedure, and a host-associated assumed-length dummy
    subroutine outer(h)
        character(len=*), intent(in) :: h
        if (len(h) /= 8) error stop 60
        call nested('abc')
    contains
        subroutine nested(t)
            character(len=*), intent(in) :: t
            if (len(h) /= 8) error stop 61
            if (h // t /= 'internalabc') error stop 62
        end subroutine
    end subroutine

    integer function next_len()
        n_calls = n_calls + 1
        next_len = n_calls + 2
    end function

    integer function len_of(s)
        character(len=*), intent(in) :: s
        len_of = len(s)
    end function

    integer function code_plus_len(s, n) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        r = iachar(s(1:1)) + len(s) + n
    end function

    integer function apply(f, s) result(r)
        procedure(str_fn) :: f
        character(len=*), intent(in) :: s
        r = f(s, 100)
    end function

    function base_describe(self, s) result(r)
        class(base_t), intent(in) :: self
        character(len=*), intent(in) :: s
        integer :: r
        r = len(s)
    end function

    function child_describe(self, s) result(r)
        class(child_t), intent(in) :: self
        character(len=*), intent(in) :: s
        integer :: r
        r = 1000 + len(s)
    end function

end module

program string_arg_hidden_length_01
    use string_arg_hidden_length_01_m
    implicit none
    character(len=7) :: s7
    character(len=5) :: d
    character(len=3) :: names(3)
    character(len=2) :: tags(4)
    character(len=6) :: packed(1)
    character(len=20) :: total
    character(len=:), allocatable :: acc
    character(len=:), pointer :: p
    character(len=4), target :: ptarget
    procedure(str_fn), pointer :: fp
    class(base_t), allocatable :: obj
    integer :: codes(3), i
    character(len=8) :: host

    s7 = 'abcdefg'
    call scalars(s7, 3, 'xyz', 'Q', d)
    if (d /= 'abxyz') error stop 30
    ! substrings and an array element as actuals
    call scalars('abcdefgh'(1:7), 3, s7(1:0) // 'xyz', names(1)(1:0) // 'Q', d)
    if (d /= 'abxyz') error stop 31

    names = ['abc', 'def', 'ghi']
    tags = ['12', '34', '56', '78']
    call arrays(names, 3, tags, total)
    if (total /= 'abc12def34ghi56') error stop 32
    if (names(3) /= 'zzz') error stop 33
    call arrays(names(2:3), 2, tags(3:4), total)
    if (total /= 'def56zzz78') error stop 34

    packed(1) = 'abcdef'
    call regroup(packed)

    call opt('first')
    call opt('first', 'second')

    codes = first_code(['a ', 'b ', 'c '])
    if (any(codes /= [99, 100, 101])) error stop 40

    if (count_down('hello') /= 5) error stop 41

    acc = 'x'
    ptarget = 'ptr!'
    p => ptarget
    call deferred(acc, p, 'yz')
    if (acc /= 'xyzptr!') error stop 42
    if (len(acc) /= 7) error stop 43

    if (assumed_shape(tags) /= 42) error stop 44
    if (by_value('A', 'four') /= 69) error stop 45

    ! The actual is evaluated once
    n_calls = 0
    call opt('first', counted('secon'))
    if (n_calls /= 1) error stop 46
    ! So is a subscript, also in a loop condition
    n_calls = 0
    if (len_of(s7(1:next_len())) /= 3) error stop 47
    if (n_calls /= 1) error stop 48
    i = 0
    do while (len_of(s7(1:next_len())) < 6)
        i = i + 1
    end do
    if (n_calls /= 4 .or. i /= 2) error stop 49

    fp => code_plus_len
    if (fp('abc', 1) /= 101) error stop 50
    if (apply(code_plus_len, 'ab') /= 199) error stop 51
    if (apply(fp, 'b') /= 199) error stop 52

    allocate(base_t :: obj)
    if (obj%describe('abc') /= 3) error stop 53
    deallocate(obj)
    allocate(child_t :: obj)
    if (obj%describe('abcd') /= 1004) error stop 54

    host = 'internal'
    call outer(host)
    call internal('abc')
    print *, "ok"

contains

    subroutine internal(t)
        character(len=*), intent(in) :: t
        if (len(t) /= 3) error stop 63
        if (host // t /= 'internalabc') error stop 64
    end subroutine

end program
