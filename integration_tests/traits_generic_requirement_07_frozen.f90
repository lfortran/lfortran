! Generic definitions whose calls with the concrete type argument Value select
! provider A's conformance to IValue, reaching generic implementations that pass
! the argument to a generic method of a runtime trait object. The runtime view
! keeps that conformance in every instantiation, whatever its client sees.
module traits_generic_requirement_07_frozen
    use traits_generic_requirement_04_contracts, only: IValue, IApply
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_04_provider_a
    implicit none

    abstract interface :: IProbe
        function probe{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function probe
    end interface IProbe

    type :: Prober
    end type Prober

    implements IProbe :: Prober
        procedure, nopass :: probe => prober_probe
    end implements

    ! Passes its argument to the runtime probe itself.
    type, implements(IApply) :: Offset
        integer :: n = 7
        class(IProbe), allocatable :: p
    contains
        procedure, pass(self) :: apply => offset_apply
    end type Offset

    ! Forwards its argument to a generic procedure that passes it on.
    type, implements(IApply) :: Relay
        class(IProbe), allocatable :: p
    contains
        procedure, pass(self) :: apply => relay_apply
    end type Relay

contains

    function prober_probe{IValue :: Q}(object) result(r)
        type(Q), intent(in) :: object
        integer :: r
        r = 10 * object%value()
    end function prober_probe

    function make_offset() result(o)
        type(Offset) :: o
        type(Prober) :: pr
        allocate(o%p, source=pr)
    end function make_offset

    function make_relay() result(o)
        type(Relay) :: o
        type(Prober) :: pr
        allocate(o%p, source=pr)
    end function make_relay

    function offset_apply{IValue :: Element}(self, object) result(r)
        class(Offset), intent(in) :: self
        type(Element), intent(in) :: object
        integer :: r
        r = self%n + object%value() + self%p%probe(object)
    end function offset_apply

    function relay_apply{IValue :: Element}(self, object) result(r)
        class(Relay), intent(in) :: self
        type(Element), intent(in) :: object
        integer :: r
        r = probed(self%p, object)
    end function relay_apply

    function probed{IValue :: E}(probe, item) result(r)
        class(IProbe), intent(in) :: probe
        type(E), intent(in) :: item
        integer :: r
        r = probe%probe(item) + item%value()
    end function probed

    function frozen{IApply :: A}(alg) result(r)
        type(A), intent(in) :: alg
        integer :: r
        r = alg%apply(Value())
    end function frozen

    function frozen_arg{IApply :: A}(alg, v) result(r)
        type(A), intent(in) :: alg
        type(Value), intent(in) :: v
        integer :: r
        r = alg%apply(v)
    end function frozen_arg

    ! A forwarded type argument takes the conformance its caller selects.
    function forwarded{IApply :: A, IValue :: Item}(alg, v) result(r)
        type(A), intent(in) :: alg
        type(Item), intent(in) :: v
        integer :: r
        r = alg%apply(v)
    end function forwarded

end module traits_generic_requirement_07_frozen
