! An abstract type that adopts a trait without binding its messages leaves
! them to its concrete extensions, as Java's abstract classes leave the
! methods of an interface they implement: a message is called through an
! object declared `class(Parent)`, as the reported `res = c%method2()` with
! `class(Parent), allocatable :: c` and `c = Child()` does, and such an
! object is passed to a trait-object dummy, through every level of abstract
! types, also with a defining receiver, a component designator and a type
! that binds one message itself.
module traits_abstract_adopter_01_m
    implicit none
    private
    public :: IDeferred, Parent, Middle, Child, Grandchild, Holder, call_it, bump_it, through_middle

    abstract interface :: IDeferred
        function method2(self) result(res)
            type(itself), pass, intent(in) :: self
            real :: res
        end function method2
        subroutine bump(self, by)
            type(itself), pass, intent(inout) :: self
            real, intent(in) :: by
        end subroutine bump
        integer function kind_of(self)
            type(itself), pass, intent(in) :: self
        end function kind_of
    end interface

    ! Binds one message itself and leaves the others to its extensions.
    type, abstract, implements(IDeferred) :: Parent
    contains
        procedure, pass :: kind_of => parent_kind_of
    end type Parent

    type, extends(Parent) :: Child
        real :: v = 4.0
    contains
        procedure, pass :: method2 => child_method2
        procedure, pass :: bump => child_bump
    end type Child

    type, abstract, extends(Parent) :: Middle
    end type Middle

    type, extends(Middle) :: Grandchild
        real :: w = 10.0
    contains
        procedure, pass :: method2 => grandchild_method2
        procedure, pass :: bump => grandchild_bump
        procedure, pass :: kind_of => grandchild_kind_of
    end type Grandchild

    type :: Holder
        class(Parent), allocatable :: item
    end type Holder

contains

    integer function parent_kind_of(self)
        class(Parent), intent(in) :: self
        parent_kind_of = 1
    end function parent_kind_of

    function child_method2(self) result(res)
        class(Child), intent(in) :: self
        real :: res
        res = self%v
    end function child_method2

    subroutine child_bump(self, by)
        class(Child), intent(inout) :: self
        real, intent(in) :: by
        self%v = self%v + by
    end subroutine child_bump

    function grandchild_method2(self) result(res)
        class(Grandchild), intent(in) :: self
        real :: res
        res = self%w
    end function grandchild_method2

    subroutine grandchild_bump(self, by)
        class(Grandchild), intent(inout) :: self
        real, intent(in) :: by
        self%w = self%w + 2 * by
    end subroutine grandchild_bump

    integer function grandchild_kind_of(self)
        class(Grandchild), intent(in) :: self
        grandchild_kind_of = 3
    end function grandchild_kind_of

    real function call_it(x)
        class(IDeferred), intent(in) :: x
        call_it = x%method2()
    end function call_it

    subroutine bump_it(x, by)
        class(Parent), intent(inout) :: x
        real, intent(in) :: by
        call x%bump(by)
    end subroutine bump_it

    real function through_middle(x)
        class(Middle), intent(in) :: x
        through_middle = x%method2() + x%kind_of()
    end function through_middle
end module traits_abstract_adopter_01_m

program traits_abstract_adopter_01
    use traits_abstract_adopter_01_m
    implicit none
    class(Parent), allocatable :: c, g
    type(Holder) :: h
    real :: res

    c = Child()
    res = c%method2()
    if (abs(res - 4.0) > 1e-6) error stop 1
    if (c%kind_of() /= 1) error stop 2
    if (abs(call_it(c) - 4.0) > 1e-6) error stop 3
    call c%bump(1.5)
    if (abs(c%method2() - 5.5) > 1e-6) error stop 4
    call bump_it(c, 0.5)
    if (abs(call_it(c) - 6.0) > 1e-6) error stop 5

    allocate(g, source=Grandchild())
    if (abs(g%method2() - 10.0) > 1e-6) error stop 6
    if (g%kind_of() /= 3) error stop 7
    call bump_it(g, 1.0)
    if (abs(call_it(g) - 12.0) > 1e-6) error stop 8
    select type (g)
    class is (Middle)
        if (abs(through_middle(g) - 15.0) > 1e-6) error stop 9
    end select

    allocate(h%item, source=Child(v=7.0))
    if (abs(h%item%method2() - 7.0) > 1e-6) error stop 10
    call h%item%bump(1.0)
    if (abs(h%item%method2() - 8.0) > 1e-6) error stop 11
    print *, res, call_it(g), h%item%method2()
end program traits_abstract_adopter_01
