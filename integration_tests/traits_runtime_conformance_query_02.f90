! Trait guard precedence (TD-11): concrete guards first, then a guard naming
! a child trait over guards naming its ancestors, then source order between
! unrelated traits, then CLASS DEFAULT.
module traits_runtime_conformance_query_02_m
    implicit none
    abstract interface :: IParent
        integer function p(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface, extends(IParent) :: IChild
        integer function c(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: IOther
        integer function o(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: IUnrelated
        integer function u(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type, implements(IChild) :: TChild
    contains
        procedure :: p => child_p
        procedure :: c => child_c
    end type
    type, implements(IParent + IOther) :: TParentOther
    contains
        procedure :: p => parent_other_p
        procedure :: o => parent_other_o
    end type
    type, implements(IChild + IOther) :: TAll
    contains
        procedure :: p => all_p
        procedure :: c => all_c
        procedure :: o => all_o
    end type
    type, implements(IOther + IUnrelated) :: TPair
    contains
        procedure :: o => pair_o
        procedure :: u => pair_u
    end type
    type :: TNone
    end type
contains
    integer function child_p(self)
        class(TChild), intent(in) :: self
        child_p = 1
    end function
    integer function child_c(self)
        class(TChild), intent(in) :: self
        child_c = 2
    end function
    integer function parent_other_p(self)
        class(TParentOther), intent(in) :: self
        parent_other_p = 11
    end function
    integer function parent_other_o(self)
        class(TParentOther), intent(in) :: self
        parent_other_o = 13
    end function
    integer function all_p(self)
        class(TAll), intent(in) :: self
        all_p = 21
    end function
    integer function all_c(self)
        class(TAll), intent(in) :: self
        all_c = 22
    end function
    integer function all_o(self)
        class(TAll), intent(in) :: self
        all_o = 23
    end function
    integer function pair_o(self)
        class(TPair), intent(in) :: self
        pair_o = 33
    end function
    integer function pair_u(self)
        class(TPair), intent(in) :: self
        pair_u = 34
    end function

    ! The child guard wins in either order.
    integer function parent_first(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IParent)
            r = 100 + x%p()
        class is (IChild)
            r = 200 + x%c()
        class default
            r = 0
        end select
    end function
    integer function child_first(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IChild)
            r = 200 + x%c()
        class is (IParent)
            r = 100 + x%p()
        class default
            r = 0
        end select
    end function

    ! Between unrelated traits, the first guard in source order wins.
    integer function other_first(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IOther)
            r = 300 + x%o()
        class is (IUnrelated)
            r = 400 + x%u()
        end select
    end function
    integer function unrelated_first(object) result(r)
        class(*), intent(in) :: object
        r = 0
        select type (x => object)
        class is (IUnrelated)
            r = 400 + x%u()
        class is (IOther)
            r = 300 + x%o()
        end select
    end function

    ! IParent precedes IOther in source order, but only until IChild, which
    ! follows both, removes it; then IOther precedes IChild. No fixed order
    ! of the three guards gives these results.
    integer function three(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IParent)
            r = 100 + x%p()
        class is (IOther)
            r = 300 + x%o()
        class is (IChild)
            r = 200 + x%c()
        class default
            r = 0
        end select
    end function

    ! A concrete guard beats a trait guard that precedes it.
    integer function concrete(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IChild)
            r = 200 + x%c()
        type is (TChild)
            r = 500
        class is (TAll)
            r = 600
        class default
            r = 0
        end select
    end function

    ! Without a matching guard and CLASS DEFAULT no block executes.
    integer function no_default(view) result(r)
        class(IParent), intent(in) :: view
        r = -1
        select type (x => view)
        class is (IOther)
            r = x%o()
        end select
    end function
end module

program traits_runtime_conformance_query_02
    use traits_runtime_conformance_query_02_m
    implicit none
    type(TChild) :: child
    type(TParentOther) :: parent_other
    type(TAll) :: everything
    type(TPair) :: pair
    type(TNone) :: none

    if (parent_first(child) /= 202 .or. child_first(child) /= 202) error stop 1
    if (parent_first(parent_other) /= 111 .or. child_first(parent_other) /= 111) error stop 2
    if (parent_first(everything) /= 222 .or. child_first(everything) /= 222) error stop 3
    if (parent_first(none) /= 0 .or. child_first(pair) /= 0) error stop 4

    if (other_first(pair) /= 333 .or. unrelated_first(pair) /= 434) error stop 5
    if (other_first(everything) /= 323 .or. unrelated_first(everything) /= 323) error stop 6
    if (unrelated_first(child) /= 0) error stop 7

    if (three(child) /= 202) error stop 8
    if (three(parent_other) /= 111) error stop 9
    if (three(everything) /= 323) error stop 10
    if (three(pair) /= 333 .or. three(none) /= 0) error stop 11

    if (concrete(child) /= 500 .or. concrete(everything) /= 600) error stop 12
    if (concrete(parent_other) /= 0) error stop 13

    if (no_default(child) /= -1 .or. no_default(parent_other) /= 13) error stop 14
    if (no_default(everything) /= 23) error stop 15
    print *, "trait guards: child over parent, source order, concrete first"
end program
