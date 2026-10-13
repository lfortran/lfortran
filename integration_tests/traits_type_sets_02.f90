! LFortran traits extension test; GFortran does not accept this syntax.
! Character members of type sets have a constant length that is part of the
! member, like the length of a character type argument: character(len=3) and
! character(len=5) are two members, each with its own specialization. Their
! common operations are the lexical comparisons; concatenation is available
! where every member gives it the same result type.
module traits_type_sets_02_m
    implicit none
    private
    public :: smallest, count_equal, doubled, first_ordered

    abstract interface :: ICode
        character(len=3) | character(len=5)
    end interface ICode

contains

    function smallest{ICode :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = x(1)
        do i = 2, size(x)
            if (x(i) < r) r = x(i)
        end do
    end function smallest

    function count_equal{ICode :: T}(x, key) result(n)
        type(T), intent(in) :: x(:), key
        integer             :: n
        integer             :: i
        n = 0
        do i = 1, size(x)
            if (x(i) == key) n = n + 1
        end do
    end function count_equal

    ! One member: x // x has the single result type character(len=4).
    function doubled{character(len=2) :: T}(x) result(r)
        type(T), intent(in) :: x
        character(len=4)    :: r
        r = x // x
    end function doubled

    function first_ordered{character(len=3) | character(len=5) :: T}(a, b) result(r)
        type(T), intent(in) :: a, b
        type(T)             :: r
        if (a <= b) then
            r = a
        else
            r = b
        end if
    end function first_ordered
end module traits_type_sets_02_m

program traits_type_sets_02
    use traits_type_sets_02_m, only: smallest, count_equal, doubled, first_ordered
    implicit none
    character(len=3) :: c3(4)
    character(len=5) :: c5(3)
    integer :: checks

    checks = 0
    c3 = ['pqr', 'abc', 'xyz', 'abd']
    c5 = ['hello', 'apple', 'zebra']

    call check(smallest(c3) == 'abc')
    call check(len(smallest(c3)) == 3)
    call check(smallest(c5) == 'apple')
    call check(len(smallest(c5)) == 5)
    call check(smallest{character(len=5)}(c5(3:2:-1)) == 'apple')
    call check(smallest(c3(4:3:-1)) == 'abd')
    call check(count_equal(c3, 'abc') == 1)
    call check(count_equal(c5, c5(3)) == 1)
    call check(count_equal(c3(1:0), 'abc') == 0)
    call check(doubled('ab') == 'abab')
    call check(first_ordered('abd', c3(2)) == 'abc')
    call check(first_ordered(c5(1), 'help!') == 'hello')

    if (checks /= 12) error stop 2
    print '(a,i0,a)', 'traits_type_sets_02: ', checks, ' checks passed'

contains

    subroutine check(condition)
        logical, intent(in) :: condition
        checks = checks + 1
        if (.not. condition) then
            print '(a,i0,a)', 'check ', checks, ' failed'
            error stop 1
        end if
    end subroutine check
end program traits_type_sets_02
