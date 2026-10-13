! Associated types and unconstrained generic storage (paper sect:tats and
! sect:associated_types_usage) without trait-method mutation: the trait's
! associated type Element is inferred per implementation (U for the generic
! Bag{U}, character(len=4) for Word, integer for Counter) and consistently
! for both messages, including a result of type Element. Bag{U} has no
! constraint (IAnyType) and stores an allocatable array of U; instances are
! inferred from initializer arguments or named explicitly, for real(real64),
! logical, character(len=5), character(len=2), integer and a derived type.
! The ordinary (non-trait) push binding reallocates repeatedly; character
! length is part of the inferred type argument; instances copy by value.
! Mutation through a trait message is covered separately by
! traits_associated_02.
module traits_associated_01_traits_m
    implicit none
    private
    public :: IPeek

    abstract interface :: IPeek
        type :: Element
        function count_after(item) result(n)
            type(Element), intent(in) :: item
            integer                   :: n
        end function count_after
        function first_or(item) result(e)
            type(Element), intent(in) :: item
            type(Element)             :: e
        end function first_or
    end interface IPeek
end module traits_associated_01_traits_m

module traits_associated_01_bag_m
    use traits_associated_01_traits_m, only: IPeek
    implicit none
    private

    type, public, sealed, implements(IPeek) :: Bag{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: init
        procedure, pass :: push
        procedure, pass :: length
        procedure, pass :: get
        procedure, pass :: count_after
        procedure, pass :: first_or
    end type Bag

contains

    function init(item) result(res)
        type(U), intent(in) :: item
        type(Bag{U})        :: res
        res%items = [item]
    end function init

    subroutine push(self, item)
        type(Bag{U}), intent(inout) :: self
        type(U),      intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push

    function length(self) result(n)
        type(Bag{U}), intent(in) :: self
        integer                  :: n
        n = size(self%items)
    end function length

    function get(self, i) result(item)
        type(Bag{U}), intent(in) :: self
        integer,      intent(in) :: i
        type(U)                  :: item
        item = self%items(i)
    end function get

    function count_after(self, item) result(n)
        type(Bag{U}), intent(in) :: self
        type(U),      intent(in) :: item
        integer                  :: n
        n = size(self%items) + 1
    end function count_after

    function first_or(self, item) result(e)
        type(Bag{U}), intent(in) :: self
        type(U),      intent(in) :: item
        type(U)                  :: e
        if (size(self%items) > 0) then
            e = self%items(1)
        else
            e = item
        end if
    end function first_or
end module traits_associated_01_bag_m

module traits_associated_01_word_m
    use traits_associated_01_traits_m, only: IPeek
    implicit none
    private

    type, public, sealed, implements(IPeek) :: Word
        character(len=4) :: text = ""
    contains
        procedure, pass :: count_after => word_count_after
        procedure, pass :: first_or => word_first_or
    end type Word

    type, public, sealed, implements(IPeek) :: Counter
        integer :: n = 0
    contains
        procedure, pass :: count_after => counter_count_after
        procedure, pass :: first_or => counter_first_or
    end type Counter

contains

    function word_count_after(self, item) result(n)
        type(Word),       intent(in) :: self
        character(len=4), intent(in) :: item
        integer                      :: n
        n = 1
        if (len_trim(self%text) > 0 .and. item /= self%text) n = 2
    end function word_count_after

    function word_first_or(self, item) result(e)
        type(Word),       intent(in) :: self
        character(len=4), intent(in) :: item
        character(len=4)             :: e
        e = item
        if (len_trim(self%text) > 0) e = self%text
    end function word_first_or

    function counter_count_after(self, item) result(n)
        type(Counter), intent(in) :: self
        integer,       intent(in) :: item
        integer                   :: n
        n = self%n + item
    end function counter_count_after

    function counter_first_or(self, item) result(e)
        type(Counter), intent(in) :: self
        integer,       intent(in) :: item
        integer                   :: e
        e = item
        if (self%n /= 0) e = self%n
    end function counter_first_or
end module traits_associated_01_word_m

module traits_associated_01_point_m
    implicit none
    type :: Point
        integer :: x = 0
        integer :: y = 0
    end type Point
end module traits_associated_01_point_m

program traits_associated_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_associated_01_bag_m, only: Bag
    use traits_associated_01_word_m, only: Word, Counter
    use traits_associated_01_point_m, only: Point
    implicit none
    integer :: checks, i, total
    type(Bag{real(real64)}) :: doubles
    type(Bag{integer}) :: ints, copy
    type(Point) :: p
    type(Word) :: w
    type(Counter) :: k

    checks = 0

    ! U inferred from the initializer argument, or named explicitly.
    doubles = Bag(0.0_real64)
    call doubles%push(1.5_real64)
    call doubles%push(2.2_real64)
    call check(doubles%length() == 3, 1)
    call check(doubles%get(1) == 0.0_real64 .and. doubles%get(2) == 1.5_real64 &
        .and. doubles%get(3) == 2.2_real64, 2)
    call check(kind(doubles%get(1)) == real64, 3)
    flags := Bag{logical}(.true.)
    call flags%push(.false.)
    call check(flags%length() == 2, 4)
    call check(flags%get(1) .and. .not. flags%get(2), 5)

    ! Character length is part of the inferred type argument.
    names := Bag("John ")
    call names%push("Mary ")
    call names%push("Anne ")
    call check(names%length() == 3 .and. len(names%get(1)) == 5, 6)
    call check(names%get(2) == "Mary " .and. names%get(3) == "Anne ", 7)
    shorts := Bag("ab")
    call shorts%push("cd")
    call check(len(shorts%get(2)) == 2 .and. shorts%get(2) == "cd", 8)

    ! Repeated reallocation, value copies and a derived element type.
    ints = Bag{integer}(7)
    do i = 1, 20
        call ints%push(i)
    end do
    total = 0
    do i = 1, ints%length()
        total = total + ints%get(i)
    end do
    call check(ints%length() == 21 .and. ints%get(21) == 20, 9)
    call check(total == 217, 10)
    copy = ints
    call copy%push(100)
    call check(ints%length() == 21 .and. copy%length() == 22, 11)
    call check(copy%get(22) == 100 .and. copy%get(1) == 7, 12)
    points := Bag(Point(1, 2))
    call points%push(Point(3, 4))
    p = points%get(2)
    call check(points%length() == 2 .and. p%x == 3 .and. p%y == 4, 13)

    ! Element inferred per implementation and shared by both messages.
    call check(doubles%count_after(9.0_real64) == 4, 14)
    call check(names%first_or("Zed  ") == "John ", 15)
    p = points%first_or(Point(9, 9))
    call check(p%x == 1 .and. p%y == 2, 16)
    w = Word("tree")
    call check(w%count_after("leaf") == 2, 17)
    call check(w%first_or("none") == "tree", 18)
    k = Counter(41)
    call check(k%count_after(1) == 42, 19)
    call check(k%first_or(5) == 41, 20)

    print '(a,i0)', 'traits_associated_01 checks: ', checks
    if (checks /= 20) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_01 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_01
