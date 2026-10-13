! Default input/output in the procedures that generic type instances copy:
! the positive controls of the temporary limitation that
! continue_compilation_traits_associated (A09) diagnoses. Every instance
! copies its package's printing and rendering procedures; instances of
! intrinsic types and of a derived type whose components can be transferred
! (fixed-size arrays and nested components included) print and render their
! items, and a package without input/output takes a type argument with an
! allocatable component.
module traits_associated_04_shelf_m
    implicit none
    private

    type, public, sealed :: Shelf{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: init
        procedure, pass :: add
        procedure, pass :: show
        procedure, pass :: render
    end type Shelf

contains

    function init(item) result(res)
        type(U), intent(in) :: item
        type(Shelf{U})      :: res
        res%items = [item]
    end function init

    subroutine add(self, item)
        type(Shelf{U}), intent(inout) :: self
        type(U),        intent(in)    :: item
        self%items = [self%items, item]
    end subroutine add

    subroutine show(self)
        type(Shelf{U}), intent(in) :: self
        print *, self%items
    end subroutine show

    function render(self) result(text)
        type(Shelf{U}), intent(in) :: self
        character(len=200)         :: text
        write(text, *) self%items
    end function render
end module traits_associated_04_shelf_m

module traits_associated_04_stack_m
    implicit none
    private

    type, public, sealed :: Stack{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: start
        procedure, pass :: push
        procedure, pass :: depth
        procedure, pass :: top
    end type Stack

contains

    function start(item) result(res)
        type(U), intent(in) :: item
        type(Stack{U})      :: res
        res%items = [item]
    end function start

    subroutine push(self, item)
        type(Stack{U}), intent(inout) :: self
        type(U),        intent(in)    :: item
        self%items = [self%items, item]
    end subroutine push

    function depth(self) result(n)
        type(Stack{U}), intent(in) :: self
        integer                    :: n
        n = size(self%items)
    end function depth

    function top(self) result(item)
        type(Stack{U}), intent(in) :: self
        type(U)                    :: item
        item = self%items(size(self%items))
    end function top
end module traits_associated_04_stack_m

module traits_associated_04_types_m
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
end module traits_associated_04_types_m

program traits_associated_04
    use traits_associated_04_shelf_m, only: Shelf
    use traits_associated_04_stack_m, only: Stack
    use traits_associated_04_types_m, only: Inner, Point, Bag
    implicit none
    integer :: checks, x1, x2, k1, k2, n1, n2
    real :: y11, y12, y21, y22
    character(len=200) :: text
    type(Bag) :: last

    checks = 0

    points := Shelf(Point(1, [2.0, 3.0], Inner(4)))
    call points%add(Point(5, [6.0, 7.0], Inner(8)))
    call points%show()
    text = points%render()
    read(text, *) x1, y11, y12, k1, x2, y21, y22, k2
    call check(x1 == 1 .and. y11 == 2.0 .and. y12 == 3.0 .and. k1 == 4, 1)
    call check(x2 == 5 .and. y21 == 6.0 .and. y22 == 7.0 .and. k2 == 8, 2)

    numbers := Shelf(10)
    call numbers%add(20)
    call numbers%show()
    text = numbers%render()
    read(text, *) n1, n2
    call check(n1 == 10 .and. n2 == 20, 3)

    names := Shelf("abc")
    call names%add("def")
    call names%show()
    text = names%render()
    call check(index(text, "abcdef") > 0, 4)

    bags := Stack(Bag([1, 2]))
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
end program traits_associated_04
