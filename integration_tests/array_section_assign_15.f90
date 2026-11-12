! An array assigned to a substring of each element of a character array
! section, `names(:)(2:3) = t` (issue #16161). The elements of the value
! are assigned one by one, and all of them are evaluated before the target
! is defined, so the value may overlap the target.
program array_section_assign_15
    implicit none
    character(len=4) :: names(3), a(3), b(3), z(0:2), m(2, 2)
    character(len=2) :: t(3)
    character(len=5) :: w(4)
    character(len=:), allocatable :: d(:)
    integer :: n

    t = ['xy', 'zw', 'uv']

    names = ['abcd', 'efgh', 'ijkl']
    names(:)(2:3) = t
    if (any(names /= ['axyd', 'ezwh', 'iuvl'])) error stop 1

    ! The value is a substring of the target itself
    names = ['abcd', 'efgh', 'ijkl']
    names(:)(2:3) = names(:)(3:4)
    if (any(names /= ['acdd', 'eghh', 'ikll'])) error stop 2

    names = ['abcd', 'efgh', 'ijkl']
    names(2:3)(1:2) = names(2:3)(3:4)
    if (any(names /= ['abcd', 'ghgh', 'klkl'])) error stop 3

    a = ['abcd', 'efgh', 'ijkl']
    b = ['mnop', 'qrst', 'uvwx']
    b(:)(1:2) = a(:)(3:4)
    if (any(b /= ['cdop', 'ghst', 'klwx'])) error stop 4

    ! The value reads the elements of the target in another order, so each
    ! of them is read before it is assigned
    names = ['abcd', 'efgh', 'ijkl']
    names(1:3)(1:2) = names(3:1:-1)(3:4)
    if (any(names /= ['klcd', 'ghgh', 'cdkl'])) error stop 5

    names = ['abcd', 'efgh', 'ijkl']
    names(2:3)(1:2) = names(1:2)(1:2)
    if (any(names /= ['abcd', 'abgh', 'efkl'])) error stop 6

    names = ['abcd', 'efgh', 'ijkl']
    names(1:2)(3:4) = names(2:3)(3:4)
    if (any(names /= ['abgh', 'efkl', 'ijkl'])) error stop 7

    ! A stride, and bounds that are not constants
    names = ['abcd', 'efgh', 'ijkl']
    names(3:1:-2)(2:3) = ['12', '34']
    if (any(names /= ['a34d', 'efgh', 'i12l'])) error stop 8

    n = 2
    names = ['abcd', 'efgh', 'ijkl']
    names(n:)(n:n + 1) = t(:n)
    if (any(names /= ['abcd', 'exyh', 'izwl'])) error stop 9

    ! The value is padded or truncated to the length of the substring
    names = ['abcd', 'efgh', 'ijkl']
    names(:)(1:3) = t
    if (any(names /= ['xy d', 'zw h', 'uv l'])) error stop 10

    names = ['abcd', 'efgh', 'ijkl']
    names(:)(4:4) = t // '!'
    if (any(names /= ['abcx', 'efgz', 'ijku'])) error stop 11

    ! A lower bound other than 1
    z = ['abcd', 'efgh', 'ijkl']
    z(:)(:2) = t
    if (any(z /= ['xycd', 'zwgh', 'uvkl'])) error stop 12

    z(1:)(3:) = [character(len=2) :: 'p', 'q']
    if (any(z /= ['xycd', 'zwp ', 'uvq '])) error stop 13

    ! One column of a rank 2 array
    m = reshape(['abcd', 'efgh', 'ijkl', 'mnop'], [2, 2])
    m(:, 2)(3:4) = t(2:3)
    if (any(m(:, 1) /= ['abcd', 'efgh'])) error stop 14
    if (any(m(:, 2) /= ['ijzw', 'mnuv'])) error stop 15

    ! An empty section
    names = ['abcd', 'efgh', 'ijkl']
    names(3:2)(1:2) = t(1:0)
    if (any(names /= ['abcd', 'efgh', 'ijkl'])) error stop 16

    ! A function result, an assumed-shape target of assumed length
    call set(w, 2, 4)
    if (any(w /= ['hello', 'hBq o', 'hello', 'hAq o'])) error stop 17

    allocate(character(len=3) :: d(2))
    d = 'abc'
    d(:)(2:2) = ['X', 'Y']
    if (any(d /= ['aXc', 'aYc'])) error stop 18

    ! The assignment is one statement: of a FORALL, of an IF, of a loop
    names = ['abcd', 'efgh', 'ijkl']
    forall (n = 1:2) names(:)(n:n) = t(:)(n:n)
    if (any(names /= ['xycd', 'zwgh', 'uvkl'])) error stop 19

    if (size(t) == 3) names(:)(3:4) = t(3:1:-1)
    if (any(names /= ['xyuv', 'zwzw', 'uvxy'])) error stop 20

    do n = 1, 2
        names(:)(n:n) = t(:)(3 - n:3 - n)
    end do
    if (any(names /= ['yxuv', 'wzzw', 'vuxy'])) error stop 21

    print *, names, w
contains

    function f(k) result(r)
        integer, intent(in) :: k
        character(len=3) :: r(k)
        integer :: j
        do j = 1, k
            r(j) = achar(64 + j) // 'q'
        end do
    end function

    subroutine set(x, lo, hi)
        character(len=*), intent(inout) :: x(:)
        integer, intent(in) :: lo, hi
        x = 'hello'
        x(size(x):1:-2)(lo:hi) = f(2)
    end subroutine

end program
