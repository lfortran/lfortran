! Generic definitions that call generic trait methods with the concrete type
! argument Value, whose conformance to IValue is visible here through
! provider A. The conformance selected where each call is checked is kept by
! every instantiation, whatever its client sees.
module traits_generic_requirement_04_frozen
    use traits_generic_requirement_04_contracts, only: IValue, IApply, IPair
    use traits_generic_requirement_04_types, only: Value
    use traits_generic_requirement_04_provider_a
    implicit none

    type, implements(IApply) :: Offset
        integer :: n = 7
    contains
        procedure, pass(self) :: apply => offset_apply
    end type Offset

    type, implements(IPair) :: Combiner
    contains
        procedure, nopass :: pair => combine
    end type Combiner

contains

    function offset_apply{IValue :: Element}(self, object) result(r)
        class(Offset), intent(in) :: self
        type(Element), intent(in) :: object
        integer :: r
        r = self%n + object%value()
    end function offset_apply

    function combine{IValue :: Left, IValue :: Right}(x, y) result(r)
        type(Left), intent(in) :: x
        type(Right), intent(in) :: y
        integer :: r
        r = 10 * x%value() + y%value()
    end function combine

    function frozen{IApply :: A}(alg) result(r)
        type(A), intent(in) :: alg
        integer :: r
        r = alg%apply(Value())
    end function frozen

    ! Both explicit forms name the same type and conformance as frozen's.
    function frozen_explicit{IApply :: A}(alg) result(r)
        type(A), intent(in) :: alg
        integer :: r
        r = alg%apply{Value}(Value()) + alg%apply{T=Value}(Value())
    end function frozen_explicit

    ! The concrete type argument of a dummy argument.
    function frozen_arg{IApply :: A}(alg, v) result(r)
        type(A), intent(in) :: alg
        type(Value), intent(in) :: v
        integer :: r
        r = alg%apply(v)
    end function frozen_arg

    ! Forwarding to frozen republishes its specialization here with the
    ! conformance frozen's definition selected.
    function relay{IApply :: B}(alg) result(r)
        type(B), intent(in) :: alg
        integer :: r
        r = frozen(alg)
    end function relay

    function frozen_pair{IPair :: Pairs}(p) result(r)
        type(Pairs), intent(in) :: p
        integer :: r
        r = p%pair(Value(), Value())
    end function frozen_pair

    ! A forwarded type argument takes the conformance its caller selects.
    function forwarded{IApply :: A, IValue :: Item}(alg, v) result(r)
        type(A), intent(in) :: alg
        type(Item), intent(in) :: v
        integer :: r
        r = alg%apply(v)
    end function forwarded

end module traits_generic_requirement_04_frozen
