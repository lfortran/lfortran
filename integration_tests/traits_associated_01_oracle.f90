! Standard-Fortran oracle for traits_associated_01. The program's instances
! of Bag{U} are written out as separate types (U = real(real64), logical,
! character(len=5), character(len=2), integer and Point), each with the
! bindings the program uses and unchanged bodies; Element is U in each
! instance, character(len=4) for Word and integer for Counter. Initializers
! inferred from real(real64) and Point arguments share the generic name Bag;
! standard generic resolution ignores character length, so the two character
! instances, like the explicitly named logical and integer instances, use
! their instance constructors. := declarations become explicit declarations.
! The checks, their order and the summary line match the extension test.
module traits_associated_01_oracle_point_m
    implicit none
    type :: Point
        integer :: x = 0
        integer :: y = 0
    end type Point
end module traits_associated_01_oracle_point_m

module traits_associated_01_oracle_bag_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_associated_01_oracle_point_m, only: Point
    implicit none
    private
    public :: Bag, Bag_real64, Bag_logical, Bag_char5, Bag_char2, Bag_integer, Bag_point

    type :: Bag_real64
        private
        real(real64), allocatable :: items(:)
    contains
        procedure, pass :: push => push_real64
        procedure, pass :: length => length_real64
        procedure, pass :: get => get_real64
        procedure, pass :: count_after => count_after_real64
    end type Bag_real64

    type :: Bag_logical
        private
        logical, allocatable :: items(:)
    contains
        procedure, pass :: push => push_logical
        procedure, pass :: length => length_logical
        procedure, pass :: get => get_logical
    end type Bag_logical

    type :: Bag_char5
        private
        character(len=5), allocatable :: items(:)
    contains
        procedure, pass :: push => push_char5
        procedure, pass :: length => length_char5
        procedure, pass :: get => get_char5
        procedure, pass :: first_or => first_or_char5
    end type Bag_char5

    type :: Bag_char2
        private
        character(len=2), allocatable :: items(:)
    contains
        procedure, pass :: push => push_char2
        procedure, pass :: get => get_char2
    end type Bag_char2

    type :: Bag_integer
        private
        integer, allocatable :: items(:)
    contains
        procedure, pass :: push => push_integer
        procedure, pass :: length => length_integer
        procedure, pass :: get => get_integer
    end type Bag_integer

    type :: Bag_point
        private
        type(Point), allocatable :: items(:)
    contains
        procedure, pass :: push => push_point
        procedure, pass :: length => length_point
        procedure, pass :: get => get_point
        procedure, pass :: first_or => first_or_point
    end type Bag_point

    interface Bag
        module procedure init_real64, init_point
    end interface Bag
    interface Bag_logical
        module procedure init_logical
    end interface Bag_logical
    interface Bag_char5
        module procedure init_char5
    end interface Bag_char5
    interface Bag_char2
        module procedure init_char2
    end interface Bag_char2
    interface Bag_integer
        module procedure init_integer
    end interface Bag_integer

contains

    function init_real64(item) result(res)
        real(real64), intent(in) :: item
        type(Bag_real64)         :: res
        res%items = [item]
    end function init_real64

    subroutine push_real64(self, item)
        class(Bag_real64), intent(inout) :: self
        real(real64),      intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_real64

    function length_real64(self) result(n)
        class(Bag_real64), intent(in) :: self
        integer                       :: n
        n = size(self%items)
    end function length_real64

    function get_real64(self, i) result(item)
        class(Bag_real64), intent(in) :: self
        integer,           intent(in) :: i
        real(real64)                  :: item
        item = self%items(i)
    end function get_real64

    function count_after_real64(self, item) result(n)
        class(Bag_real64), intent(in) :: self
        real(real64),      intent(in) :: item
        integer                       :: n
        n = size(self%items) + 1
    end function count_after_real64

    function init_logical(item) result(res)
        logical, intent(in) :: item
        type(Bag_logical)   :: res
        res%items = [item]
    end function init_logical

    subroutine push_logical(self, item)
        class(Bag_logical), intent(inout) :: self
        logical,            intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_logical

    function length_logical(self) result(n)
        class(Bag_logical), intent(in) :: self
        integer                        :: n
        n = size(self%items)
    end function length_logical

    function get_logical(self, i) result(item)
        class(Bag_logical), intent(in) :: self
        integer,            intent(in) :: i
        logical                        :: item
        item = self%items(i)
    end function get_logical

    function init_char5(item) result(res)
        character(len=5), intent(in) :: item
        type(Bag_char5)              :: res
        res%items = [item]
    end function init_char5

    subroutine push_char5(self, item)
        class(Bag_char5), intent(inout) :: self
        character(len=5), intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_char5

    function length_char5(self) result(n)
        class(Bag_char5), intent(in) :: self
        integer                      :: n
        n = size(self%items)
    end function length_char5

    function get_char5(self, i) result(item)
        class(Bag_char5), intent(in) :: self
        integer,          intent(in) :: i
        character(len=5)             :: item
        item = self%items(i)
    end function get_char5

    function first_or_char5(self, item) result(e)
        class(Bag_char5), intent(in) :: self
        character(len=5), intent(in) :: item
        character(len=5)             :: e
        if (size(self%items) > 0) then
            e = self%items(1)
        else
            e = item
        end if
    end function first_or_char5

    function init_char2(item) result(res)
        character(len=2), intent(in) :: item
        type(Bag_char2)              :: res
        res%items = [item]
    end function init_char2

    subroutine push_char2(self, item)
        class(Bag_char2), intent(inout) :: self
        character(len=2), intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_char2

    function get_char2(self, i) result(item)
        class(Bag_char2), intent(in) :: self
        integer,          intent(in) :: i
        character(len=2)             :: item
        item = self%items(i)
    end function get_char2

    function init_integer(item) result(res)
        integer, intent(in) :: item
        type(Bag_integer)   :: res
        res%items = [item]
    end function init_integer

    subroutine push_integer(self, item)
        class(Bag_integer), intent(inout) :: self
        integer,            intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_integer

    function length_integer(self) result(n)
        class(Bag_integer), intent(in) :: self
        integer                        :: n
        n = size(self%items)
    end function length_integer

    function get_integer(self, i) result(item)
        class(Bag_integer), intent(in) :: self
        integer,            intent(in) :: i
        integer                        :: item
        item = self%items(i)
    end function get_integer

    function init_point(item) result(res)
        type(Point), intent(in) :: item
        type(Bag_point)         :: res
        res%items = [item]
    end function init_point

    subroutine push_point(self, item)
        class(Bag_point), intent(inout) :: self
        type(Point),      intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_point

    function length_point(self) result(n)
        class(Bag_point), intent(in) :: self
        integer                      :: n
        n = size(self%items)
    end function length_point

    function get_point(self, i) result(item)
        class(Bag_point), intent(in) :: self
        integer,          intent(in) :: i
        type(Point)                  :: item
        item = self%items(i)
    end function get_point

    function first_or_point(self, item) result(e)
        class(Bag_point), intent(in) :: self
        type(Point),      intent(in) :: item
        type(Point)                  :: e
        if (size(self%items) > 0) then
            e = self%items(1)
        else
            e = item
        end if
    end function first_or_point
end module traits_associated_01_oracle_bag_m

module traits_associated_01_oracle_word_m
    implicit none
    private

    type, public :: Word
        character(len=4) :: text = ""
    contains
        procedure, pass :: count_after => word_count_after
        procedure, pass :: first_or => word_first_or
    end type Word

    type, public :: Counter
        integer :: n = 0
    contains
        procedure, pass :: count_after => counter_count_after
        procedure, pass :: first_or => counter_first_or
    end type Counter

contains

    function word_count_after(self, item) result(n)
        class(Word),      intent(in) :: self
        character(len=4), intent(in) :: item
        integer                      :: n
        n = 1
        if (len_trim(self%text) > 0 .and. item /= self%text) n = 2
    end function word_count_after

    function word_first_or(self, item) result(e)
        class(Word),      intent(in) :: self
        character(len=4), intent(in) :: item
        character(len=4)             :: e
        e = item
        if (len_trim(self%text) > 0) e = self%text
    end function word_first_or

    function counter_count_after(self, item) result(n)
        class(Counter), intent(in) :: self
        integer,        intent(in) :: item
        integer                    :: n
        n = self%n + item
    end function counter_count_after

    function counter_first_or(self, item) result(e)
        class(Counter), intent(in) :: self
        integer,        intent(in) :: item
        integer                    :: e
        e = item
        if (self%n /= 0) e = self%n
    end function counter_first_or
end module traits_associated_01_oracle_word_m

program traits_associated_01_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_associated_01_oracle_bag_m, only: Bag, Bag_real64, Bag_logical, Bag_char5, &
        Bag_char2, Bag_integer, Bag_point
    use traits_associated_01_oracle_word_m, only: Word, Counter
    use traits_associated_01_oracle_point_m, only: Point
    implicit none
    integer :: checks, i, total
    type(Bag_real64) :: doubles
    type(Bag_integer) :: ints, copy
    type(Bag_logical) :: flags
    type(Bag_char5) :: names
    type(Bag_char2) :: shorts
    type(Bag_point) :: points
    type(Point) :: p
    type(Word) :: w
    type(Counter) :: k

    checks = 0

    doubles = Bag(0.0_real64)
    call doubles%push(1.5_real64)
    call doubles%push(2.2_real64)
    call check(doubles%length() == 3, 1)
    call check(doubles%get(1) == 0.0_real64 .and. doubles%get(2) == 1.5_real64 &
        .and. doubles%get(3) == 2.2_real64, 2)
    call check(kind(doubles%get(1)) == real64, 3)
    flags = Bag_logical(.true.)
    call flags%push(.false.)
    call check(flags%length() == 2, 4)
    call check(flags%get(1) .and. .not. flags%get(2), 5)

    names = Bag_char5("John ")
    call names%push("Mary ")
    call names%push("Anne ")
    call check(names%length() == 3 .and. len(names%get(1)) == 5, 6)
    call check(names%get(2) == "Mary " .and. names%get(3) == "Anne ", 7)
    shorts = Bag_char2("ab")
    call shorts%push("cd")
    call check(len(shorts%get(2)) == 2 .and. shorts%get(2) == "cd", 8)

    ints = Bag_integer(7)
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
    points = Bag(Point(1, 2))
    call points%push(Point(3, 4))
    p = points%get(2)
    call check(points%length() == 2 .and. p%x == 3 .and. p%y == 4, 13)

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
end program traits_associated_01_oracle
