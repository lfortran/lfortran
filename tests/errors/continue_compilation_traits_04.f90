! Generic derived types: located diagnostics for invalid uses. Each case is
! isolated in its own scoping unit so continue-compilation reports exactly
! one diagnostic per case without cascades. Cases marked "prototype
! boundary" diagnose current restrictions of this prototype, not permanent
! language rules.
module gt_neg_contracts
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function value
    end interface IValue
    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, pass :: sum => simple_sum
    end type SimpleSum
    type, sealed, implements(ISum) :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass :: sum => scaled_sum
    end type ScaledSum
    ! No conformance to ISum.
    type :: Plain
        integer :: n = 0
    end type Plain
contains
    function simple_sum{INumeric :: T}(self, x) result(s)
        type(SimpleSum), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum
    function scaled_sum{INumeric :: T}(self, x) result(s)
        type(ScaledSum), intent(in) :: self
        type(T),         intent(in) :: x(:)
        type(T)                     :: s
        s = T(self%factor) * simple_sum(SimpleSum(), x)
    end function scaled_sum
end module gt_neg_contracts

module gt_neg_generic
    use gt_neg_contracts, only: INumeric, ISum
    implicit none
    private
    public :: PairwiseSum, Holder
    type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
        private
        type(U), allocatable :: other
    contains
        initial :: init
        procedure, pass :: sum
    end type PairwiseSum
    ! U appears in no component and no initializer dummy.
    type :: Holder{ISum :: U}
        integer :: n = 0
    end type Holder
contains
    function init(other) result(res)
        type(U), intent(in)  :: other
        type(PairwiseSum{U}) :: res
        res%other = other
    end function init
    function sum{INumeric :: T}(self, x) result(s)
        type(PairwiseSum{U}), intent(in) :: self
        type(T),              intent(in) :: x(:)
        type(T)                          :: s
        s = self%other%sum(x)
    end function sum
end module gt_neg_generic

! N01: declared type argument without visible ISum conformance.
module gt_neg_01
    use gt_neg_contracts, only: Plain
    use gt_neg_generic, only: PairwiseSum
    implicit none
    type(PairwiseSum{Plain}) :: p01
end module gt_neg_01

! N02: intrinsic type argument for a universal-trait parameter.
module gt_neg_02
    use gt_neg_generic, only: PairwiseSum
    implicit none
    type(PairwiseSum{integer}) :: p02
end module gt_neg_02

! N03: an inferred parameter must satisfy its constraint too.
module gt_neg_03
    use gt_neg_contracts, only: Plain
    use gt_neg_generic, only: PairwiseSum
    implicit none
contains
    subroutine n03()
        p03 := PairwiseSum(Plain())
    end subroutine n03
end module gt_neg_03

! N04: explicit and inferred type arguments conflict.
module gt_neg_04
    use gt_neg_contracts, only: SimpleSum, ScaledSum
    use gt_neg_generic, only: PairwiseSum
    implicit none
contains
    subroutine n04()
        p04 := PairwiseSum{SimpleSum}(ScaledSum(2))
    end subroutine n04
end module gt_neg_04

! N05: different instances of one generic type are different types.
module gt_neg_05
    use gt_neg_contracts, only: SimpleSum, ScaledSum
    use gt_neg_generic, only: PairwiseSum
    implicit none
contains
    subroutine n05()
        type(PairwiseSum{SimpleSum}) :: p05
        p05 = PairwiseSum(ScaledSum(2))
    end subroutine n05
end module gt_neg_05

! N06: a parameter determined by no actual is not guessed.
module gt_neg_06
    use gt_neg_generic, only: Holder
    implicit none
contains
    subroutine n06()
        h06 := Holder(n = 3)
    end subroutine n06
end module gt_neg_06

! N07: wrong number of type arguments.
module gt_neg_07
    use gt_neg_contracts, only: SimpleSum
    use gt_neg_generic, only: PairwiseSum
    implicit none
    type(PairwiseSum{SimpleSum, SimpleSum}) :: p07
end module gt_neg_07

! N08: a generic type is not a type without its arguments.
module gt_neg_08
    use gt_neg_generic, only: PairwiseSum
    implicit none
    type(PairwiseSum) :: p08
end module gt_neg_08

! N09: type arguments for a nongeneric type.
module gt_neg_09
    use gt_neg_contracts, only: SimpleSum
    implicit none
    type(SimpleSum{integer}) :: p09
end module gt_neg_09

! N10: a type argument must name a type.
module gt_neg_10
    use gt_neg_generic, only: PairwiseSum
    implicit none
    type(PairwiseSum{3}) :: p10
end module gt_neg_10

! N11 (paper rule, traits.tex 2343-2351): an initializer inherits its type's
! parameters and must not declare them.
module gt_neg_11
    use gt_neg_contracts, only: ISum
    implicit none
    type :: BadInit{ISum :: U}
        type(U), allocatable :: item
    contains
        initial :: make
    end type BadInit
contains
    function make{ISum :: U}(item) result(res)
        type(U), intent(in) :: item
        type(BadInit{U})    :: res
        res%item = item
    end function make
end module gt_neg_11

! N12 (prototype boundary): a method binder may not reuse the name of a
! parameter of its containing type.
module gt_neg_12
    use gt_neg_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Shadow{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure, pass :: sum
    end type Shadow
contains
    function sum{INumeric :: U}(self, x) result(s)
        type(Shadow{U}), intent(in) :: self
        type(U),         intent(in) :: x(:)
        type(U)                     :: s
        s = self%item%sum(x)
    end function sum
end module gt_neg_12

! N13 (prototype boundary): an implements statement repeats the type's
! parameter list exactly.
module gt_neg_13
    use gt_neg_contracts, only: INumeric, ISum, IValue
    implicit none
    type :: Mismatch{ISum :: U}
        type(U) :: item
    end type Mismatch
    implements ISum :: Mismatch{IValue :: U}
        procedure, pass :: sum
    end implements Mismatch
contains
    function sum{INumeric :: T}(self, x) result(s)
        class(Mismatch{U}), intent(in) :: self
        type(T),            intent(in) :: x(:)
        type(T)                        :: s
        s = self%item%sum(x)
    end function sum
end module gt_neg_13

! N14 (prototype boundary): an implements statement for a generic type
! repeats its parameter list; omitting it is not supported.
module gt_neg_14
    use gt_neg_contracts, only: INumeric, ISum
    implicit none
    type :: Bare{ISum :: U}
        type(U) :: item
    end type Bare
    implements ISum :: Bare
        procedure, pass :: sum
    end implements Bare
contains
    function sum{INumeric :: T}(self, x) result(s)
        class(Bare{U}), intent(in) :: self
        type(T),        intent(in) :: x(:)
        type(T)                    :: s
        s = self%item%sum(x)
    end function sum
end module gt_neg_14

! N15 (prototype boundary): a parameter-dependent implementation procedure
! (its declarations use the binding type's parameters) belongs to exactly one
! generic type. A parameter-independent routine could still be bound to both
! types and called by name, as in ordinary Fortran.
module gt_neg_15
    use gt_neg_contracts, only: ISum
    implicit none
    type :: First{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure, nopass :: describe
    end type First
    type :: Second{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure, nopass :: describe
    end type Second
contains
    integer function describe(item) result(r)
        type(U), intent(in) :: item
        r = 1
    end function describe
end module gt_neg_15

! N16 (boundary): extending a generic type is not implemented.
module gt_neg_16
    use gt_neg_generic, only: PairwiseSum
    implicit none
    type, extends(PairwiseSum) :: Child
    end type Child
end module gt_neg_16

! N18 (boundary): generic types are declared in modules in this milestone.
program gt_neg_18
    use gt_neg_contracts, only: ISum
    implicit none
    type :: LocalBox{ISum :: U}
        type(U), allocatable :: item
    end type LocalBox
end program gt_neg_18

! N19 (prototype boundary): an alpha-renamed parameter list in an implements
! statement does not repeat the type's list.
module gt_neg_19
    use gt_neg_contracts, only: INumeric, ISum
    implicit none
    type :: Renamed{ISum :: U}
        type(U) :: item
    end type Renamed
    implements ISum :: Renamed{ISum :: V}
        procedure, pass :: sum
    end implements Renamed
contains
    function sum{INumeric :: T}(self, x) result(s)
        class(Renamed{U}), intent(in) :: self
        type(T),           intent(in) :: x(:)
        type(T)                       :: s
        s = self%item%sum(x)
    end function sum
end module gt_neg_19

! N20 (prototype boundary): an initializer has no generic parameters of its
! own.
module gt_neg_20
    use gt_neg_contracts, only: INumeric, ISum
    implicit none
    type :: Extra{ISum :: U}
        type(U), allocatable :: item
    contains
        initial :: make
    end type Extra
contains
    function make{INumeric :: V}(item, n) result(res)
        type(U), intent(in) :: item
        type(V), intent(in) :: n
        type(Extra{U})      :: res
        res%item = item
    end function make
end module gt_neg_20

! N21 (prototype boundary): a procedure that depends on the type's
! parameters is not called by name.
module gt_neg_21
    use gt_neg_contracts, only: SimpleSum, ISum
    implicit none
    type :: Direct{ISum :: U}
        type(U), allocatable :: item
    contains
        initial :: wrap
    end type Direct
contains
    function wrap(item) result(res)
        type(U), intent(in) :: item
        type(Direct{U})     :: res
        res%item = item
    end function wrap
    subroutine n21()
        print *, allocated(wrap(SimpleSum()))
    end subroutine n21
end module gt_neg_21

! N22 (prototype boundary): nor is it imported by name.
module gt_neg_22
    use gt_neg_generic, only: init
    implicit none
end module gt_neg_22

! N23 (prototype boundary): a procedure bound to a generic type that does not
! use its parameters stays an ordinary module procedure; binding one is not
! implemented yet.
module gt_neg_23
    use gt_neg_contracts, only: ISum
    implicit none
    type :: Described{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure, nopass :: describe
    end type Described
contains
    integer function describe()
        describe = 23
    end function describe
end module gt_neg_23

! N24 (boundary): FINAL procedures of generic types are not implemented.
module gt_neg_24
    use gt_neg_contracts, only: ISum
    implicit none
    type :: Finalized{ISum :: U}
        type(U), allocatable :: item
    contains
        final :: release
    end type Finalized
contains
    subroutine release(n)
        integer, intent(inout) :: n
        n = 0
    end subroutine release
end module gt_neg_24

! N25 (boundary): GENERIC bindings of generic types are not implemented.
module gt_neg_25
    use gt_neg_contracts, only: INumeric, ISum
    implicit none
    type :: Overloaded{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure, pass :: sum
        generic :: total => sum
    end type Overloaded
contains
    function sum{INumeric :: T}(self, x) result(s)
        class(Overloaded{U}), intent(in) :: self
        type(T),              intent(in) :: x(:)
        type(T)                          :: s
        s = self%item%sum(x)
    end function sum
end module gt_neg_25

! N26 (boundary): DEFERRED bindings of generic types are not implemented.
module gt_neg_26
    use gt_neg_contracts, only: ISum
    implicit none
    abstract interface
        subroutine run_iface()
        end subroutine run_iface
    end interface
    type, abstract :: Deferred{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure(run_iface), deferred, nopass :: run
    end type Deferred
end module gt_neg_26
