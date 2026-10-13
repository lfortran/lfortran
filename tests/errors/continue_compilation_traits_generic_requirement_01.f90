! Generic methods of generic type parameter constraints: each generic body is
! checked at its definition against the method's own constraints, even when
! the generic procedure is never instantiated. Each case has its own scoping
! unit so continue-compilation reports exactly one diagnostic per case.
module traits_generic_requirement_contracts
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
    abstract interface :: IOtherNumeric
        integer | real(real64)
    end interface IOtherNumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
        function zero{INumeric :: T}() result(z)
            type(T) :: z
        end function zero
        function pair{INumeric :: T, INumeric :: U}(x, y) result(r)
            type(T), intent(in) :: x
            type(U), intent(in) :: y
            integer :: r
        end function pair
    end interface ISum
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    abstract interface :: IOther
        function other() result(r)
            integer :: r
        end function other
    end interface IOther
    abstract interface :: IApply
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function apply
    end interface IApply
    type :: Widget
        integer :: w = 0
    end type Widget
end module

module traits_generic_requirement_member
    use traits_generic_requirement_contracts
    implicit none
contains
    function complex_sum{ISum :: Adder}(a, c) result(r)
        type(Adder), intent(in) :: a
        complex(8), intent(in) :: c(:)
        complex(8) :: r
        r = a%sum(c)
    end function
end module

module traits_generic_requirement_distinct_set
    use traits_generic_requirement_contracts
    implicit none
contains
    function distinct_sum{ISum :: Adder, IOtherNumeric :: E}(a, x) result(r)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x(:)
        type(E) :: r
        r = a%sum(x)
    end function
end module

module traits_generic_requirement_implication
    use traits_generic_requirement_contracts
    implicit none
contains
    function unimplied{IApply :: Algorithm, IOther :: V}(alg, object) result(r)
        type(Algorithm), intent(in) :: alg
        type(V), intent(in) :: object
        integer :: r
        r = alg%apply(object)
    end function
end module

module traits_generic_requirement_conformance
    use traits_generic_requirement_contracts
    implicit none
contains
    function unconforming{IApply :: Algorithm}(alg, object) result(r)
        type(Algorithm), intent(in) :: alg
        type(Widget), intent(in) :: object
        integer :: r
        r = alg%apply(object)
    end function
end module

module traits_generic_requirement_unknown
    use traits_generic_requirement_contracts
    implicit none
contains
    function unknown_method{ISum :: Adder, INumeric :: E}(a, x) result(r)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x(:)
        type(E) :: r
        r = a%product(x)
    end function
end module

module traits_generic_requirement_infer
    use traits_generic_requirement_contracts
    implicit none
contains
    function uninferred{ISum :: Adder, INumeric :: E}(a, x) result(r)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x
        type(E) :: r
        r = x + a%zero()
    end function
end module

module traits_generic_requirement_count
    use traits_generic_requirement_contracts
    implicit none
contains
    function miscounted{ISum :: Adder, INumeric :: E}(a, x) result(r)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x(:)
        type(E) :: r
        r = a%sum{E, E}(x)
    end function
end module

module traits_generic_requirement_keyword
    use traits_generic_requirement_contracts
    implicit none
contains
    function misnamed{ISum :: Adder, INumeric :: E}(a, x) result(r)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x(:)
        type(E) :: r
        r = a%sum{U=E}(x)
    end function
end module

module traits_generic_requirement_mixed
    use traits_generic_requirement_contracts
    implicit none
contains
    function mixed_pair{ISum :: Adder, INumeric :: E}(a, x) result(r)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x
        integer :: r
        r = a%pair(x, 1)
    end function
end module

module traits_generic_requirement_category
    use traits_generic_requirement_contracts
    implicit none
contains
    subroutine called_function{ISum :: Adder, INumeric :: E}(a, x)
        type(Adder), intent(in) :: a
        type(E), intent(in) :: x(:)
        call a%sum(x)
    end subroutine
end module

! A generic message whose own type parameter is constrained by a trait with
! generic methods would need a nested requirement.
module traits_generic_requirement_nested
    use traits_generic_requirement_contracts
    implicit none
    abstract interface :: IVisit
        function visit{ISum :: S}(summer) result(r)
            type(S), intent(in) :: summer
            integer :: r
        end function visit
    end interface IVisit
end module
