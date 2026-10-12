! Located diagnostics for the open runtime of protocol traits: a runtime
! generic message erases a binder constrained by a protocol, and each call
! passes the dictionary of its type argument's conformance. Each numbered
! case is an independent violation; all other code is valid. A conformance
! whose generic procedure cannot be erased stays valid static code: forming
! its runtime view, in cc_traits_ocp_views, is the violation.
module cc_traits_ocp_contracts
    implicit none
    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: IScale
        elemental function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface IScale

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: ISquares
        function squares{IScale :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function squares
    end interface ISquares
end module cc_traits_ocp_contracts

module cc_traits_ocp_bag
    use cc_traits_ocp_contracts, only: INumeric
    implicit none
    type, sealed :: Bag
        integer, allocatable :: v(:)
    end type Bag
    implements INumeric :: Bag
        initial :: bag_of
        procedure, static :: operator(+) => joined
    end implements Bag
contains
    function bag_of(n) result(res)
        integer, intent(in) :: n
        type(Bag)           :: res
        res%v = [n]
    end function bag_of
    function joined(lhs, rhs) result(res)
        type(Bag), intent(in) :: lhs, rhs
        type(Bag)             :: res
        res%v = [lhs%v, rhs%v]
    end function joined
end module cc_traits_ocp_bag

module cc_traits_ocp_provider
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Simple
    contains
        procedure, nopass :: sum => simple_sum
    end type Simple
contains
    function simple_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum
end module cc_traits_ocp_provider

! E01: the erased binder's local array has no storage its dictionary describes.
module cc_traits_ocp_storage
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Buffered
    contains
        procedure, nopass :: sum => buffered_sum
    end type Buffered
contains
    function buffered_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        type(T)             :: copy(size(x))
        copy = x
        s = copy(1)
    end function buffered_sum
end module cc_traits_ocp_storage

! E02: an elemental requirement applied to erased arrays computes an array.
module cc_traits_ocp_elemental
    use cc_traits_ocp_contracts, only: IScale, ISquares
    implicit none
    type, sealed, implements(ISquares) :: Squarer
    contains
        procedure, nopass :: squares => first_square
    end type Squarer
contains
    function first{IScale :: U}(y) result(s)
        type(U), intent(in) :: y(:)
        type(U)             :: s
        s = y(1)
    end function first
    function first_square{IScale :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = first(x * x)
    end function first_square
end module cc_traits_ocp_elemental

module cc_traits_ocp_integer
    use cc_traits_ocp_contracts, only: INumeric
    implicit none
    implements INumeric :: integer
    end implements integer
end module cc_traits_ocp_integer

program continue_compilation_traits_ocp
    use cc_traits_ocp_contracts, only: INumeric, ISum
    use cc_traits_ocp_bag, only: Bag
    use cc_traits_ocp_provider, only: Simple
    implicit none
    implements INumeric :: real
    end implements real
    class(ISum), allocatable :: p
    type(Bag) :: b(2), rb
    real :: r(3)
    integer :: k(3)
    p = Simple()
    ! E03: Bag's values own an allocatable component, so its conformance
    ! has no dictionary.
    rb = p%sum(b)
    ! E04: the program's conformance of real has no dictionary.
    print *, p%sum(r)
    ! E05: integer's conformance is declared in a module the program does
    ! not use.
    print *, p%sum(k)
end program continue_compilation_traits_ocp

! E06-E10: a runtime generic message may only designate, assign, pass and
! inquire the shape of erased values; each of these bodies needs more.
module cc_traits_ocp_uses
    use cc_traits_ocp_contracts, only: INumeric, ISum
    use cc_traits_ocp_provider, only: simple_sum
    implicit none
    type, sealed, implements(ISum) :: Gathered
    contains
        procedure, nopass :: sum => gathered_sum
    end type Gathered
    type, sealed, implements(ISum) :: Merged
    contains
        procedure, nopass :: sum => merged_sum
    end type Merged
    type, sealed, implements(ISum) :: Shown
    contains
        procedure, nopass :: sum => shown_sum
    end type Shown
    type, sealed, implements(ISum) :: Printed
    contains
        procedure, nopass :: sum => printed_sum
    end type Printed
    type, sealed, implements(ISum) :: Bounded
    contains
        procedure, nopass :: sum => bounded_sum
    end type Bounded
contains
    subroutine show(v)
        class(*), intent(in) :: v
    end subroutine show
    function gathered_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        ! E06: a vector subscript gathers a copy of erased values.
        s = simple_sum(x([1, 3]))
    end function gathered_sum
    function merged_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        ! E07: an intrinsic procedure computes an erased value.
        s = merge(x(1), x(2), size(x) > 1)
    end function merged_sum
    function shown_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
        ! E08: an unlimited polymorphic dummy needs a typed value.
        call show(s)
    end function shown_sum
    function printed_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
        ! E09: output of an erased value.
        print *, s
    end function printed_sum
    function bounded_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        ! E10: the bounds of a section are inquired from a copy of it.
        s = x(ubound(x(2:), 1))
    end function bounded_sum
end module cc_traits_ocp_uses

module cc_traits_ocp_internal
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Nested
    contains
        procedure, nopass :: sum => nested_sum
    end type Nested
contains
    function nested_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = T(0)
        call accumulate()
    contains
        ! E11: an internal procedure of a dictionary-erased body.
        subroutine accumulate()
            s = s + x(1)
        end subroutine accumulate
    end function nested_sum
end module cc_traits_ocp_internal

! E12: a protocol with a requirement that a dictionary cannot pass has no
! runtime dictionaries; its static uses stay valid.
module cc_traits_ocp_static_protocols
    implicit none
    abstract interface :: IWeigh
        function operator(*)(lhs, v) result(res)
            type(itself), intent(in) :: lhs
            type(itself), intent(in) :: v(:)
            type(itself)             :: res
        end function operator(*)
    end interface IWeigh
    abstract interface :: IReduce
        function reduce{IWeigh :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function reduce
    end interface IReduce
contains
    subroutine reduce_view()
        ! E12: IWeigh's operator(*) takes an array of type(itself), so the
        ! message of IReduce has no runtime view.
        class(IReduce), allocatable :: r
    end subroutine reduce_view
end module cc_traits_ocp_static_protocols

! E13-E14: forwarding a binder of a derived protocol where its parent is
! required would need the parent's dictionary.
module cc_traits_ocp_field
    use cc_traits_ocp_contracts, only: INumeric, ISum
    use cc_traits_ocp_provider, only: simple_sum
    implicit none
    abstract interface, extends(INumeric) :: IField
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface IField
    abstract interface :: IScale
        function scaled{IField :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function scaled
    end interface IScale
    type, sealed, implements(IScale) :: Helped
    contains
        procedure, nopass :: scaled => helped_scaled
    end type Helped
    type, sealed, implements(IScale) :: Forwarded
        class(ISum), allocatable :: inner
    contains
        procedure, pass :: scaled => forwarded_scaled
    end type Forwarded
contains
    function helped_scaled{IField :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        ! E13: to a generic procedure.
        s = simple_sum(x) * x(1)
    end function helped_scaled
    function forwarded_scaled{IField :: T}(self, x) result(s)
        class(Forwarded), intent(in) :: self
        type(T),          intent(in) :: x(:)
        type(T)                      :: s
        ! E14: to a runtime generic message.
        s = self%inner%sum(x) * x(1)
    end function forwarded_scaled
end module cc_traits_ocp_field

module cc_traits_ocp_polymorphic
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type :: Count
        integer :: n = 0
    end type Count
    implements INumeric :: Count
        initial :: count_of
        procedure, static :: operator(+) => count_add
    end implements Count
contains
    function count_of(n) result(res)
        integer, intent(in) :: n
        type(Count)         :: res
        res%n = n
    end function count_of
    function count_add(lhs, rhs) result(res)
        type(Count), intent(in) :: lhs, rhs
        type(Count)             :: res
        res%n = lhs%n + rhs%n
    end function count_add
    function total(p, c) result(r)
        class(ISum),  intent(in) :: p
        class(Count), intent(in) :: c(:)
        type(Count)              :: r
        ! E15: a polymorphic actual argument for an erased dummy.
        r = p%sum(c)
    end function total
end module cc_traits_ocp_polymorphic

! E01, E02, E06-E11, E13, E14: runtime views of the conformances above.
module cc_traits_ocp_views
    use cc_traits_ocp_contracts, only: ISum, ISquares
    use cc_traits_ocp_storage, only: Buffered
    use cc_traits_ocp_elemental, only: Squarer
    use cc_traits_ocp_uses, only: Gathered, Merged, Shown, Printed, Bounded
    use cc_traits_ocp_internal, only: Nested
    use cc_traits_ocp_field, only: IScale, Helped, Forwarded
    implicit none
contains
    subroutine views()
        class(ISum), allocatable     :: s
        class(ISquares), allocatable :: q
        class(IScale), allocatable   :: f
        s = Buffered()   ! E01
        q = Squarer()    ! E02
        s = Gathered()   ! E06
        s = Merged()     ! E07
        s = Shown()      ! E08
        s = Printed()    ! E09
        s = Bounded()    ! E10
        s = Nested()     ! E11
        f = Helped()     ! E13
        f = Forwarded()  ! E14
    end subroutine views
end module cc_traits_ocp_views

! E16: a runtime view formed before the body of its generic procedure is
! checked once all bodies exist.
module cc_traits_ocp_forward
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Later
    contains
        procedure, nopass :: sum => later_sum
    end type Later
contains
    subroutine early_view(s)
        class(ISum), allocatable, intent(out) :: s
        s = Later()
    end subroutine early_view
    function later_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
        print *, s
    end function later_sum
end module cc_traits_ocp_forward
