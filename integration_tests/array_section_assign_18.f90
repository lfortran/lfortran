! The bounds of the section and of the substring of an assignment to a
! substring of each element of a character array section,
! `names(lb:ub:st)(l:r) = value`, are evaluated once, before any element is
! defined.
program array_section_assign_18
    implicit none
    character(len=4) :: names(4)
    character(len=2) :: t(4), c
    integer :: calls, k

    t = ['xy', 'zw', 'uv', 'qr']
    c = 'cc'

    ! Every bound is a function that counts its calls. An array value:
    call reset()
    names(f(1):f(4):f(1))(f(2):f(3)) = t
    if (calls /= 5) error stop 1
    if (any(names /= ['axyd', 'ezwh', 'iuvl', 'mqrp'])) error stop 2

    call reset()
    names(f(4):f(1):f(-2))(f(1):f(2)) = t(1:2)
    if (calls /= 5) error stop 3
    if (any(names /= ['abcd', 'zwgh', 'ijkl', 'xyop'])) error stop 4

    ! A scalar variable
    call reset()
    names(f(1):f(4):f(2))(f(3):f(4)) = c
    if (calls /= 5) error stop 5
    if (any(names /= ['abcc', 'efgh', 'ijcc', 'mnop'])) error stop 6

    ! A constant
    call reset()
    names(f(2):f(3):f(1))(f(1):f(2)) = 'kk'
    if (calls /= 5) error stop 7
    if (any(names /= ['abcd', 'kkgh', 'kkkl', 'mnop'])) error stop 8

    ! A bound that is an expression
    call reset()
    names(f(1) + 1:f(2) + 1)(f(4) - f(1):4) = t(3:)
    if (calls /= 4) error stop 9
    if (any(names /= ['abcd', 'efuv', 'ijqr', 'mnop'])) error stop 10

    ! A bound whose value changes with every call: the lower bound, the
    ! upper bound and the stride of the section
    call reset()
    names(next():3)(2:3) = t(1:2)
    if (k /= 2) error stop 11
    if (any(names /= ['abcd', 'exyh', 'izwl', 'mnop'])) error stop 12

    call reset()
    names(1:next())(3:4) = t(1:2)
    if (k /= 2) error stop 13
    if (any(names /= ['abxy', 'efzw', 'ijkl', 'mnop'])) error stop 14

    call reset()
    names(::next())(1:2) = t(1:2)
    if (k /= 2) error stop 15
    if (any(names /= ['xycd', 'efgh', 'zwkl', 'mnop'])) error stop 16

    ! The lower and the upper bound of the substring
    call reset()
    names(:)(next():3) = t
    if (k /= 2) error stop 17
    if (any(names /= ['axyd', 'ezwh', 'iuvl', 'mqrp'])) error stop 18

    call reset()
    names(:)(1:next()) = t
    if (k /= 2) error stop 19
    if (any(names /= ['xycd', 'zwgh', 'uvkl', 'qrop'])) error stop 20

    call reset()
    names(:)(next():3) = c
    if (k /= 2) error stop 21
    if (any(names /= ['accd', 'ecch', 'iccl', 'mccp'])) error stop 22

    call reset()
    names(:)(next():3) = 'kk'
    if (k /= 2) error stop 23
    if (any(names /= ['akkd', 'ekkh', 'ikkl', 'mkkp'])) error stop 24

    ! A bound that depends on the target is evaluated before the first
    ! element is defined: `len_trim(names(1))` is 2 here, and would be 4
    ! after `names(1)(2:4)` is defined.
    call reset()
    names(1) = 'ab'
    names(:)(len_trim(names(1)):4) = t // '!'
    if (any(names /= ['axy!', 'ezw!', 'iuv!', 'mqr!'])) error stop 25

    call reset()
    names(1) = 'ab'
    names(:)(len_trim(names(1)):4) = c // '!'
    if (any(names /= ['acc!', 'ecc!', 'icc!', 'mcc!'])) error stop 26

    call reset()
    names(1) = 'ab'
    names(:)(len_trim(names(1)):4) = 'kk!'
    if (any(names /= ['akk!', 'ekk!', 'ikk!', 'mkk!'])) error stop 27

contains

    subroutine reset()
        names = ['abcd', 'efgh', 'ijkl', 'mnop']
        calls = 0
        k = 1
    end subroutine

    integer function f(i)
        integer, intent(in) :: i
        calls = calls + 1
        f = i
    end function

    integer function next()
        k = k + 1
        next = k
    end function

end program
