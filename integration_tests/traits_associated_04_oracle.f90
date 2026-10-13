! Standard-Fortran oracle of traits_associated_04: each generic type instance
! is written out as an ordinary derived type with its own procedures.
module traits_associated_04_oracle_types_m
    implicit none

    type :: Inner
        integer :: k = 0
    end type Inner

    type :: Point
        integer     :: x = 0
        real        :: y(2) = 0
        type(Inner) :: tag
    end type Point

    type :: Bag
        integer, allocatable :: a(:)
    end type Bag
end module traits_associated_04_oracle_types_m

module traits_associated_04_oracle_shelves_m
    use traits_associated_04_oracle_types_m, only: Point, Bag
    implicit none
    private
    public :: ShelfPoint, ShelfInteger, ShelfCharacter3, StackBag

    type :: ShelfPoint
        type(Point), allocatable :: items(:)
    contains
        procedure :: add => add_point
        procedure :: show => show_point
        procedure :: render => render_point
    end type ShelfPoint

    type :: ShelfInteger
        integer, allocatable :: items(:)
    contains
        procedure :: add => add_integer
        procedure :: show => show_integer
        procedure :: render => render_integer
    end type ShelfInteger

    type :: ShelfCharacter3
        character(len=3), allocatable :: items(:)
    contains
        procedure :: add => add_character3
        procedure :: show => show_character3
        procedure :: render => render_character3
    end type ShelfCharacter3

    type :: StackBag
        type(Bag), allocatable :: items(:)
    contains
        procedure :: push => push_bag
        procedure :: depth => depth_bag
        procedure :: top => top_bag
    end type StackBag

contains

    subroutine add_point(self, item)
        class(ShelfPoint), intent(inout) :: self
        type(Point),       intent(in)    :: item
        self%items = [self%items, item]
    end subroutine add_point

    subroutine show_point(self)
        class(ShelfPoint), intent(in) :: self
        print *, self%items
    end subroutine show_point

    function render_point(self) result(text)
        class(ShelfPoint), intent(in) :: self
        character(len=200)            :: text
        write(text, *) self%items
    end function render_point

    subroutine add_integer(self, item)
        class(ShelfInteger), intent(inout) :: self
        integer,             intent(in)    :: item
        self%items = [self%items, item]
    end subroutine add_integer

    subroutine show_integer(self)
        class(ShelfInteger), intent(in) :: self
        print *, self%items
    end subroutine show_integer

    function render_integer(self) result(text)
        class(ShelfInteger), intent(in) :: self
        character(len=200)              :: text
        write(text, *) self%items
    end function render_integer

    subroutine add_character3(self, item)
        class(ShelfCharacter3), intent(inout) :: self
        character(len=3),       intent(in)    :: item
        self%items = [self%items, item]
    end subroutine add_character3

    subroutine show_character3(self)
        class(ShelfCharacter3), intent(in) :: self
        print *, self%items
    end subroutine show_character3

    function render_character3(self) result(text)
        class(ShelfCharacter3), intent(in) :: self
        character(len=200)                 :: text
        write(text, *) self%items
    end function render_character3

    subroutine push_bag(self, item)
        class(StackBag), intent(inout) :: self
        type(Bag),       intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push_bag

    function depth_bag(self) result(n)
        class(StackBag), intent(in) :: self
        integer                     :: n
        n = size(self%items)
    end function depth_bag

    function top_bag(self) result(item)
        class(StackBag), intent(in) :: self
        type(Bag)                   :: item
        item = self%items(size(self%items))
    end function top_bag
end module traits_associated_04_oracle_shelves_m

program traits_associated_04_oracle
    use traits_associated_04_oracle_types_m, only: Inner, Point, Bag
    use traits_associated_04_oracle_shelves_m, only: ShelfPoint, ShelfInteger, &
        ShelfCharacter3, StackBag
    implicit none
    integer :: checks, x1, x2, k1, k2, n1, n2
    real :: y11, y12, y21, y22
    character(len=200) :: text
    type(ShelfPoint) :: points
    type(ShelfInteger) :: numbers
    type(ShelfCharacter3) :: names
    type(StackBag) :: bags
    type(Bag) :: last

    checks = 0

    points%items = [Point(1, [2.0, 3.0], Inner(4))]
    call points%add(Point(5, [6.0, 7.0], Inner(8)))
    call points%show()
    text = points%render()
    read(text, *) x1, y11, y12, k1, x2, y21, y22, k2
    call check(x1 == 1 .and. y11 == 2.0 .and. y12 == 3.0 .and. k1 == 4, 1)
    call check(x2 == 5 .and. y21 == 6.0 .and. y22 == 7.0 .and. k2 == 8, 2)

    numbers%items = [10]
    call numbers%add(20)
    call numbers%show()
    text = numbers%render()
    read(text, *) n1, n2
    call check(n1 == 10 .and. n2 == 20, 3)

    names%items = ["abc"]
    call names%add("def")
    call names%show()
    text = names%render()
    call check(index(text, "abcdef") > 0, 4)

    bags%items = [Bag([1, 2])]
    call bags%push(Bag([3]))
    last = bags%top()
    call check(bags%depth() == 2 .and. size(last%a) == 1 .and. last%a(1) == 3, 5)

    print '(a,i0)', 'traits_associated_04 checks: ', checks
    if (checks /= 5) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_04 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_04_oracle
