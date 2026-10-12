! Located diagnostics for initializer and operator requirements on itself,
! static bindings, intrinsic acknowledgements and explicit provider
! visibility. Each numbered case is an independent violation; all other code
! is valid.
module cc_traits_protocols_contracts
    implicit none
    abstract interface :: IRing
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer,      intent(in) :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IRing

    abstract interface :: IShow
        function code() result(c)
            integer :: c
        end function code
    end interface IShow
end module cc_traits_protocols_contracts

! E01: operator(+) is required but not bound.
module cc_traits_protocols_missing_operator
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    type, sealed :: NoPlus
        integer :: n = 0
    end type NoPlus
    implements IRing :: NoPlus
        initial :: make
        procedure, static :: operator(*) => times
    end implements NoPlus
contains
    function make(n) result(res)
        integer, intent(in) :: n
        type(NoPlus)        :: res
        res%n = n
    end function make
    function times(lhs, rhs) result(res)
        type(NoPlus), intent(in) :: lhs
        integer,      intent(in) :: rhs
        type(NoPlus)             :: res
        res%n = lhs%n * rhs
    end function times
end module cc_traits_protocols_missing_operator

! E02: the right operand of operator(+) must be type(itself), not integer.
module cc_traits_protocols_not_itself
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    type, sealed :: Skewed
        integer :: n = 0
    end type Skewed
    implements IRing :: Skewed
        initial :: make
        procedure, static :: operator(+) => plus_integer
        procedure, static :: operator(*) => times
    end implements Skewed
contains
    function make(n) result(res)
        integer, intent(in) :: n
        type(Skewed)        :: res
        res%n = n
    end function make
    function plus_integer(lhs, rhs) result(res)
        type(Skewed), intent(in) :: lhs
        integer,      intent(in) :: rhs
        type(Skewed)             :: res
        res%n = lhs%n + rhs
    end function plus_integer
    function times(lhs, rhs) result(res)
        type(Skewed), intent(in) :: lhs
        integer,      intent(in) :: rhs
        type(Skewed)             :: res
        res%n = lhs%n * rhs
    end function times
end module cc_traits_protocols_not_itself

! E03: a static binding has no passed-object argument.
module cc_traits_protocols_static_pass
    use cc_traits_protocols_contracts, only: IShow
    implicit none
    type, sealed :: Shown
        integer :: n = 0
    end type Shown
    implements IShow :: Shown
        procedure, static, pass :: code => shown_code
    end implements Shown
contains
    function shown_code(self) result(c)
        type(Shown), intent(in) :: self
        integer                 :: c
        c = self%n
    end function shown_code
end module cc_traits_protocols_static_pass

! E04: the intrinsic operator(+) of integer cannot be redefined.
module cc_traits_protocols_intrinsic_operator
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    implements IRing :: integer
        procedure, static :: operator(+) => integer_plus
    end implements integer
contains
    function integer_plus(lhs, rhs) result(res)
        integer, intent(in) :: lhs, rhs
        integer             :: res
        res = lhs - rhs
    end function integer_plus
end module cc_traits_protocols_intrinsic_operator

! E05: the intrinsic initializer of real(real64) cannot be redefined.
module cc_traits_protocols_intrinsic_initializer
    use, intrinsic :: iso_fortran_env, only: real64
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    implements IRing :: real(real64)
        initial :: real64_from_integer
    end implements real(real64)
contains
    function real64_from_integer(n) result(res)
        integer, intent(in) :: n
        real(real64)        :: res
        res = n + 1
    end function real64_from_integer
end module cc_traits_protocols_intrinsic_initializer

module cc_traits_protocols_acknowledged
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    implements IRing :: integer
    end implements integer
end module cc_traits_protocols_acknowledged

! E12: two static procedures match the same requirement.
module cc_traits_protocols_ambiguous_binding
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    type, sealed :: Twin
        integer :: n = 0
    end type Twin
    implements IRing :: Twin
        initial :: make
        procedure, static :: operator(+) => add_a, add_b
        procedure, static :: operator(*) => times
    end implements Twin
contains
    function make(n) result(res)
        integer, intent(in) :: n
        type(Twin)          :: res
        res%n = n
    end function make
    function add_a(lhs, rhs) result(res)
        type(Twin), intent(in) :: lhs, rhs
        type(Twin)             :: res
        res%n = lhs%n + rhs%n
    end function add_a
    function add_b(lhs, rhs) result(res)
        type(Twin), intent(in) :: lhs, rhs
        type(Twin)             :: res
        res%n = rhs%n + lhs%n
    end function add_b
    function times(lhs, rhs) result(res)
        type(Twin), intent(in) :: lhs
        integer,    intent(in) :: rhs
        type(Twin)             :: res
        res%n = lhs%n * rhs
    end function times
end module cc_traits_protocols_ambiguous_binding

! E13: an operator binding must be static; E14: a static operator binding
! that implements no requirement; E15: an operand that is not read-only.
module cc_traits_protocols_operator_bindings
    use cc_traits_protocols_contracts, only: IRing
    implicit none
    type, sealed :: Unbound
        integer :: n = 0
    end type Unbound
    type, sealed :: Extra
        integer :: n = 0
    end type Extra
    type, sealed :: Mutating
        integer :: n = 0
    end type Mutating
    implements IRing :: Unbound
        initial :: make_unbound
        procedure :: operator(+) => add_unbound
        procedure, static :: operator(*) => times_unbound
    end implements Unbound
    implements IRing :: Extra
        initial :: make_extra
        procedure, static :: operator(+) => add_extra
        procedure, static :: operator(*) => times_extra
        procedure, static :: operator(-) => minus_extra
    end implements Extra
    implements IRing :: Mutating
        initial :: make_mutating
        procedure, static :: operator(+) => add_mutating
        procedure, static :: operator(*) => times_mutating
    end implements Mutating
contains
    function make_unbound(n) result(res)
        integer, intent(in) :: n
        type(Unbound)       :: res
        res%n = n
    end function make_unbound
    function add_unbound(lhs, rhs) result(res)
        type(Unbound), intent(in) :: lhs, rhs
        type(Unbound)             :: res
        res%n = lhs%n + rhs%n
    end function add_unbound
    function times_unbound(lhs, rhs) result(res)
        type(Unbound), intent(in) :: lhs
        integer,       intent(in) :: rhs
        type(Unbound)             :: res
        res%n = lhs%n * rhs
    end function times_unbound
    function make_extra(n) result(res)
        integer, intent(in) :: n
        type(Extra)         :: res
        res%n = n
    end function make_extra
    function add_extra(lhs, rhs) result(res)
        type(Extra), intent(in) :: lhs, rhs
        type(Extra)             :: res
        res%n = lhs%n + rhs%n
    end function add_extra
    function times_extra(lhs, rhs) result(res)
        type(Extra), intent(in) :: lhs
        integer,     intent(in) :: rhs
        type(Extra)             :: res
        res%n = lhs%n * rhs
    end function times_extra
    function minus_extra(lhs, rhs) result(res)
        type(Extra), intent(in) :: lhs, rhs
        type(Extra)             :: res
        res%n = lhs%n - rhs%n
    end function minus_extra
    function make_mutating(n) result(res)
        integer, intent(in) :: n
        type(Mutating)      :: res
        res%n = n
    end function make_mutating
    function add_mutating(lhs, rhs) result(res)
        type(Mutating), intent(inout) :: lhs
        type(Mutating), intent(in)    :: rhs
        type(Mutating)                :: res
        lhs%n = lhs%n + rhs%n
        res%n = lhs%n
    end function add_mutating
    function times_mutating(lhs, rhs) result(res)
        type(Mutating), intent(in) :: lhs
        integer,        intent(in) :: rhs
        type(Mutating)             :: res
        res%n = lhs%n * rhs
    end function times_mutating
end module cc_traits_protocols_operator_bindings

! E16: no initializer is bound; E17: initializers are bound only in the
! module that declares the type.
module cc_traits_protocols_initializers
    use cc_traits_protocols_contracts, only: IRing
    use cc_traits_protocols_ambiguous_binding, only: Twin
    implicit none
    type, sealed :: NoInit
        integer :: n = 0
    end type NoInit
    implements IRing :: NoInit
        procedure, static :: operator(+) => add_noinit
        procedure, static :: operator(*) => times_noinit
    end implements NoInit
    implements IRing :: Twin
        initial :: twin_from
    end implements Twin
contains
    function add_noinit(lhs, rhs) result(res)
        type(NoInit), intent(in) :: lhs, rhs
        type(NoInit)             :: res
        res%n = lhs%n + rhs%n
    end function add_noinit
    function times_noinit(lhs, rhs) result(res)
        type(NoInit), intent(in) :: lhs
        integer,      intent(in) :: rhs
        type(NoInit)             :: res
        res%n = lhs%n * rhs
    end function times_noinit
    function twin_from(n) result(res)
        integer, intent(in) :: n
        type(Twin)          :: res
        res = Twin(n)
    end function twin_from
end module cc_traits_protocols_initializers

module cc_traits_protocols_declarations
    implicit none
    ! E18: an operator requirement needs an operand of type itself.
    abstract interface :: INoItself
        function operator(+)(lhs, rhs) result(res)
            integer, intent(in) :: lhs, rhs
            integer             :: res
        end function operator(+)
    end interface INoItself
    ! E19: the same requirement twice.
    abstract interface :: ITwice
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface ITwice
    ! E20: type(itself) in an ordinary message.
    abstract interface :: ICompare
        function same(other) result(r)
            type(itself), intent(in) :: other
            logical                  :: r
        end function same
    end interface ICompare
    ! Elemental and nonelemental operator(*) with equal operands.
    abstract interface :: IScale
        elemental function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer,      intent(in) :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IScale
    abstract interface :: IScaleScalar
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer,      intent(in) :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IScaleScalar
contains
    ! E21: both requirements of the composed constraint accept x * 2.
    function scaled{IScale + IScaleScalar :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x * 2
    end function scaled
end module cc_traits_protocols_declarations

module cc_traits_protocols_generic
    use cc_traits_protocols_contracts, only: IRing
    implicit none
contains
    ! E06: a trait with the associated type itself is a constraint only.
    subroutine show_ring(x)
        class(IRing), intent(in) :: x
    end subroutine show_ring

    ! E07: IRing requires no operator(/).
    function halve{IRing :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x / x
    end function halve

    ! E08: the initializer requirement takes an integer, not a real.
    function from_real{IRing :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = T(2.5) + x
    end function from_real

    ! E09: no operator(*) of IRing takes a real right operand.
    function scale_real{IRing :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x * 2.0
    end function scale_real

    function twice{IRing :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x + x
    end function twice
end module cc_traits_protocols_generic

program continue_compilation_traits_protocols
    use cc_traits_protocols_generic, only: twice
    implicit none
    integer :: i
    real :: x
    ! E10: real has no IRing conformance.
    x = twice(2.0)
    ! E11: integer's IRing conformance is declared in a module this program
    ! does not use.
    i = twice(3)
    print *, i, x
end program continue_compilation_traits_protocols

! E22: the static attribute outside an implements statement.
module cc_traits_protocols_type_static
    implicit none
    type :: Counter
        integer :: n = 7
    contains
        procedure, static :: show
    end type Counter
contains
    subroutine show(x)
        class(Counter), intent(in) :: x
        print *, x%n
    end subroutine show
end module cc_traits_protocols_type_static

! E23, E24: operator bindings, static or not, outside an implements
! statement; a type's own operator is a GENERIC binding.
module cc_traits_protocols_type_operator
    implicit none
    type :: Scaled
        integer :: n = 7
    contains
        procedure, static :: operator(*) => times_scaled
    end type Scaled
    type :: Summed
        integer :: n = 7
    contains
        procedure :: operator(+) => plus_summed
    end type Summed
    type :: Ordinary
        integer :: n = 7
    contains
        procedure, pass :: times_ordinary
        generic :: operator(*) => times_ordinary
    end type Ordinary
contains
    function times_scaled(lhs, rhs) result(res)
        type(Scaled), intent(in) :: lhs, rhs
        type(Scaled)             :: res
        res%n = lhs%n * rhs%n
    end function times_scaled
    function plus_summed(lhs, rhs) result(res)
        class(Summed), intent(in) :: lhs, rhs
        type(Summed)              :: res
        res%n = lhs%n + rhs%n
    end function plus_summed
    function times_ordinary(lhs, rhs) result(res)
        class(Ordinary), intent(in) :: lhs, rhs
        type(Ordinary)              :: res
        res%n = lhs%n * rhs%n
    end function times_ordinary
end module cc_traits_protocols_type_operator

! E25: a type adopting a trait binds its operator requirements only in an
! implements statement (E25 also reports the resulting missing binding).
module cc_traits_protocols_adopted_operator
    implicit none
    abstract interface :: IScale
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface IScale
    type, sealed, implements(IScale) :: Adopted
        integer :: n = 7
    contains
        procedure, static :: operator(*) => times_adopted
    end type Adopted
contains
    function times_adopted(lhs, rhs) result(res)
        type(Adopted), intent(in) :: lhs, rhs
        type(Adopted)             :: res
        res%n = lhs%n * rhs%n
    end function times_adopted
end module cc_traits_protocols_adopted_operator

! E26, E27, E28: the operands of an operator requirement are nonoptional
! data objects with intent(in) or the value attribute, as for a defined
! operator; a value operand is valid.
module cc_traits_protocols_operands
    implicit none
    abstract interface :: IAccumulate
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(inout) :: lhs
            type(itself), intent(in)    :: rhs
            type(itself)                :: res
        end function operator(+)
    end interface IAccumulate
    abstract interface :: IReset
        function operator(-)(lhs, rhs) result(res)
            type(itself), intent(in)  :: lhs
            type(itself), intent(out) :: rhs
            type(itself)              :: res
        end function operator(-)
    end interface IReset
    abstract interface :: IDefault
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in)      :: lhs
            integer, intent(in), optional :: rhs
            type(itself)                  :: res
        end function operator(*)
    end interface IDefault
    abstract interface :: IByValue
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer, value           :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IByValue
end module cc_traits_protocols_operands

! E29, E30: an initializer argument that the initializer may modify needs a
! definable actual argument; a definable one is valid.
module cc_traits_protocols_initializer_actuals
    implicit none
    abstract interface :: ICount
        initial(n)
            integer, intent(inout) :: n
        end initial
    end interface ICount
contains
    function from_literal{ICount :: T}() result(r)
        type(T) :: r
        r = T(1)
    end function from_literal
    function from_input{ICount :: T}(k) result(r)
        integer, intent(in) :: k
        type(T)             :: r
        r = T(k)
    end function from_input
    function from_counter{ICount :: T}(k) result(r)
        integer, intent(inout) :: k
        type(T)                :: r
        r = T(k)
    end function from_counter
end module cc_traits_protocols_initializer_actuals
