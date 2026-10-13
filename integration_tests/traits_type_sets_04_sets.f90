! Character type sets with the length wildcard and the character kind wildcard,
! compiled separately from traits_type_sets_04.f90: their members, the
! witnesses of their operations and generics constrained by them are read
! back from the module file.
module traits_type_sets_04_sets
    implicit none
    private
    public :: IText, IAlsoText, ILetter, ICountOrText, IOrder, Least, smallest, &
        count_equal, ordered, last_letter, differ, same_text, first_code

    ! A character member of any length; character(*) and character(len=*)
    ! are the same member.
    abstract interface :: IText
        character(*) | character(len=*)
    end interface IText

    ! The wildcard absorbs a constant length of its kind.
    abstract interface :: IAlsoText
        character(len=3) | character(len=*)
    end interface IAlsoText

    ! Character of length 1 and every kind of CHARACTER_KINDS.
    abstract interface :: ILetter
        character(kind=*)
    end interface ILetter

    ! Integers and characters of any length share the relational operators.
    abstract interface :: ICountOrText
        integer | character*(*)
    end interface ICountOrText

    abstract interface :: IOrder
        function least{IText :: T}(x) result(r)
            type(T), intent(in) :: x(:)
            type(T)             :: r
        end function least
    end interface IOrder

    type, sealed, implements(IOrder) :: Least
    contains
        procedure, nopass :: least => least_of
    end type Least

contains

    function smallest{IText :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = x(1)
        do i = 2, size(x)
            if (x(i) < r) r = x(i)
        end do
    end function smallest

    function count_equal{IAlsoText :: T}(x, key) result(n)
        type(T), intent(in) :: x(:), key
        integer             :: n, i
        n = 0
        do i = 1, size(x)
            if (x(i) == key) n = n + 1
        end do
    end function count_equal

    function ordered{ICountOrText :: T}(x) result(ok)
        type(T), intent(in) :: x(:)
        logical             :: ok
        integer             :: i
        ok = .true.
        do i = 2, size(x)
            if (x(i - 1) > x(i)) ok = .false.
            if (.not. (x(i) >= x(i - 1))) ok = .false.
        end do
    end function ordered

    function last_letter{ILetter :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = x(1)
        do i = 2, size(x)
            if (x(i) >= r) r = x(i)
        end do
    end function last_letter

    function differ{character(len=*, kind=*) :: T}(a, b) result(r)
        type(T), intent(in) :: a, b
        logical             :: r
        r = a /= b .and. (a < b .or. a > b) .and. .not. (a <= b .and. a >= b)
    end function differ

    ! The keyword type parameters in either order.
    function same_text{character(kind=*, len=*) :: T}(a, b) result(r)
        type(T), intent(in) :: a, b
        logical             :: r
        r = a == b
    end function same_text

    ! A positional length and a positional kind wildcard.
    function first_code{character(3, *) :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        r = x(1)
    end function first_code

    function least_of{IText :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        r = smallest(x)
    end function least_of
end module traits_type_sets_04_sets
