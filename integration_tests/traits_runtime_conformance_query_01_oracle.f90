! The ordinary abstract-type counterpart of traits_runtime_conformance_query_01:
! CLASS IS guards naming abstract parents instead of traits, for the selectors
! and inherited overrides that single inheritance can express.
module traits_runtime_conformance_query_01_oracle_m
    implicit none
    type, abstract :: IValue
        integer :: n = 0
    contains
        procedure(value_interface), deferred :: value
    end type
    abstract interface
        integer function value_interface(self)
            import :: IValue
            class(IValue), intent(in) :: self
        end function
    end interface
    type, abstract, extends(IValue) :: IScaled
    contains
        procedure(scaled_interface), deferred :: scaled
    end type
    abstract interface
        integer function scaled_interface(self, k)
            import :: IScaled
            class(IScaled), intent(in) :: self
            integer, intent(in) :: k
        end function
    end interface
    type, extends(IValue) :: Leaf
    contains
        procedure :: value => leaf_value
    end type
    type, extends(Leaf) :: Twig
    contains
        procedure :: value => twig_value
    end type
    type, extends(IScaled) :: Rich
    contains
        procedure :: value => rich_value
        procedure :: scaled => rich_scaled
    end type
    type :: Plain
        integer :: n = 0
    end type
    type :: Holder
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

    integer function via_any(object) result(r)
        class(*), intent(in) :: object
        select type (object)
        class is (IScaled)
            r = object%scaled(2)
        class is (IValue)
            r = object%value()
        class default
            r = -1
        end select
    end function

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
            class is (IScaled)
                r = r + y%scaled(1) * 100000
            end select
        end select
    end function
end module

program traits_runtime_conformance_query_01_oracle
    use traits_runtime_conformance_query_01_oracle_m
    implicit none
    type(Leaf), target :: a
    type(Twig), target :: b
    type(Rich), target :: c
    type(Plain) :: d
    class(*), allocatable :: any
    class(*), pointer :: any_pointer
    class(IValue), allocatable :: owner
    type(Holder) :: h
    a%n = 1; b%n = 2; c%n = 3; d%n = 4

    if (via_any(a) /= 1 .or. via_any(b) /= 102) error stop 1
    if (via_any(c) /= 6 .or. via_any(d) /= -1 .or. via_any(5) /= -1) error stop 2
    if (nested(a) /= 20001 .or. nested(b) /= 10102) error stop 3
    if (nested(c) /= 301003 .or. nested(d) /= -1) error stop 4
    allocate(any, source=b)
    if (via_any(any) /= 102 .or. nested(any) /= 10102) error stop 5
    any_pointer => c
    select type (any_pointer)
    class is (IScaled)
        if (any_pointer%scaled(5) /= 15) error stop 6
    class default
        error stop 7
    end select
    allocate(owner, source=c)
    select type (x => owner)
    class is (IScaled)
        if (x%scaled(3) /= 9) error stop 8
    class default
        error stop 9
    end select
    allocate(h%any, source=b)
    allocate(h%item, source=a)
    select type (x => h%any)
    class is (IScaled)
        error stop 10
    class is (IValue)
        if (x%value() /= 102) error stop 11
    end select
    select type (x => h%item)
    type is (Leaf)
        if (x%value() /= 1) error stop 12
    class default
        error stop 13
    end select
    print *, "abstract class guards: selectors, inherited overrides and nesting"
end program
