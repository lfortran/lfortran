! The consumer, compiled without the provider: default constructors copy views,
! owners and trait components of types it does not know.
module traits_runtime_constructor_component_separate_01_consumer_m
    use traits_runtime_constructor_component_separate_01_contracts_m, only: &
        IShape, IName, Holder, Duo
    implicit none
contains
    function wrap(view, tag) result(res)
        class(IShape), intent(in) :: view
        integer, intent(in) :: tag
        type(Holder) :: res
        res = Holder(view, tag)
    end function
    function wrap_owner(owner) result(res)
        class(IShape), allocatable, intent(in) :: owner
        class(*), allocatable :: res
        res = Holder(tobj=owner, tag=2)
    end function
    function pair(first, named) result(res)
        class(IShape), intent(in) :: first
        class(IShape+IName), intent(in) :: named
        type(Duo) :: res
        res = Duo(first, named)
    end function
    ! The first component is a projection of the named one.
    function twice(named) result(res)
        class(IShape+IName), allocatable, intent(in) :: named
        type(Duo) :: res
        res = Duo(named, named)
    end function
    real function total(h)
        type(Holder), intent(in) :: h
        total = -1
        if (allocated(h%tobj)) total = h%tobj%area()
    end function
    ! Copies the component of another object, then changes the original.
    subroutine copy_and_scale(h, copy)
        type(Holder), intent(inout) :: h
        type(Holder), intent(out) :: copy
        copy = Holder(h%tobj, h%tag + 10)
        call h%tobj%scale(2.0)
    end subroutine
end module
