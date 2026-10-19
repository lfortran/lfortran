! CLASS IS (trait) queries the conformance of the dynamic type of trait
! objects, class(*) and class(Base) selectors (TD-11). Extensions inherit the
! conformances declared with their parent type, with their own overrides.
module traits_runtime_conformance_query_01_m
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: ILabel
        integer function label(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface, extends(IValue) :: IScaled
        integer function scaled(self, k)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: k
        end function
    end interface
    type :: Base
        integer :: n = 0
    end type
    type, extends(Base), implements(IValue) :: Leaf
    contains
        procedure :: value => leaf_value
    end type
    type, extends(Leaf) :: Twig
    contains
        procedure :: value => twig_value
    end type
    type, extends(Base), implements(IScaled + ILabel) :: Rich
    contains
        procedure :: value => rich_value
        procedure :: scaled => rich_scaled
        procedure :: label => rich_label
    end type
    type, extends(Base) :: Plain
    end type
    type :: Holder
        class(Base), allocatable :: base
        class(*), allocatable :: any
        class(IValue), allocatable :: item
    end type
contains
    integer function leaf_value(self)
        class(Leaf), intent(in) :: self
        leaf_value = self%n
    end function
    integer function twig_value(self)
        class(Twig), intent(in) :: self
        twig_value = self%n + 100
    end function
    integer function rich_value(self)
        class(Rich), intent(in) :: self
        rich_value = self%n + 1000
    end function
    integer function rich_scaled(self, k)
        class(Rich), intent(in) :: self
        integer, intent(in) :: k
        rich_scaled = self%n * k
    end function
    integer function rich_label(self)
        class(Rich), intent(in) :: self
        rich_label = 7
    end function

    integer function via_base(object) result(r)
        class(Base), intent(in) :: object
        select type (x => object)
        type is (Plain)
            r = -2
        class is (IValue)
            r = x%value()
        class default
            r = -1
        end select
    end function

    integer function via_any(object) result(r)
        class(*), intent(in) :: object
        select type (object)
        class is (ILabel)
            r = object%label()
        class is (IValue)
            r = object%value()
        class default
            r = -1
        end select
    end function

    ! Inside the guard the associate name keeps the dynamic type.
    integer function nested(object) result(r)
        class(*), intent(in) :: object
        r = -1
        select type (x => object)
        class is (IValue)
            r = x%value()
            select type (y => x)
            type is (Twig)
                r = r + 10000
            class is (Leaf)
                r = r + 20000
            class is (ILabel)
                r = r + y%label() * 100000
            end select
        end select
    end function

    integer function via_view(view) result(r)
        class(IValue), intent(in) :: view
        select type (x => view)
        type is (Leaf)
            r = -5
        class is (ILabel)
            r = x%label() + view%value()
        class default
            r = 0
        end select
    end function

    integer function via_owner(owner) result(r)
        class(IValue), allocatable, intent(in) :: owner
        select type (x => owner)
        class is (IScaled)
            r = x%scaled(2)
        class default
            r = -1
        end select
    end function
end module

program traits_runtime_conformance_query_01
    use traits_runtime_conformance_query_01_m
    implicit none
    type(Leaf), target :: a
    type(Twig), target :: b
    type(Rich), target :: c
    type(Plain) :: d
    class(*), allocatable :: any
    class(*), pointer :: any_pointer
    class(Base), allocatable :: base_owner
    class(IValue), allocatable :: owner
    class(IValue), pointer :: view
    type(Holder) :: h
    integer :: r
    a%n = 1; b%n = 2; c%n = 3; d%n = 4

    if (via_base(a) /= 1 .or. via_base(b) /= 102) error stop 1
    if (via_base(c) /= 1003 .or. via_base(d) /= -2) error stop 2
    if (via_any(a) /= 1 .or. via_any(b) /= 102) error stop 3
    if (via_any(c) /= 7 .or. via_any(d) /= -1 .or. via_any(5) /= -1) error stop 4
    if (nested(a) /= 20001 .or. nested(b) /= 10102) error stop 5
    if (nested(c) /= 701003 .or. nested(d) /= -1) error stop 6
    if (via_view(a) /= -5 .or. via_view(b) /= 0 .or. via_view(c) /= 1010) error stop 7

    allocate(any, source=b)
    if (via_any(any) /= 102 .or. nested(any) /= 10102) error stop 8
    any_pointer => c
    if (via_any(any_pointer) /= 7) error stop 9
    select type (any_pointer)
    class is (IScaled)
        if (any_pointer%scaled(5) /= 15) error stop 10
    class default
        error stop 11
    end select
    allocate(base_owner, source=a)
    if (via_base(base_owner) /= 1) error stop 12
    select type (x => base_owner)
    class is (ILabel)
        error stop 13
    class is (IValue)
        if (x%value() /= 1) error stop 14
    end select

    owner = c
    if (via_owner(owner) /= 6) error stop 15
    select type (x => owner)
    class is (ILabel)
        if (x%label() /= 7) error stop 16
    class default
        error stop 17
    end select
    owner = b
    if (via_owner(owner) /= -1) error stop 18
    view => c
    r = 0
    select type (x => view)
    type is (Leaf)
        error stop 19
    class is (ILabel)
        r = x%label()
    end select
    if (r /= 7) error stop 20

    allocate(h%base, source=c)
    allocate(h%any, source=b)
    allocate(Rich :: h%item)
    h%item = c
    select type (x => h%base)
    class is (IScaled)
        if (x%scaled(3) /= 9) error stop 21
    class default
        error stop 22
    end select
    select type (x => h%any)
    class is (ILabel)
        error stop 23
    class is (IValue)
        if (x%value() /= 102) error stop 24
    end select
    select type (x => h%item)
    class is (ILabel)
        if (x%label() /= 7) error stop 25
    class default
        error stop 26
    end select
    print *, "trait guards: selectors, inherited conformances and nesting"
end program
