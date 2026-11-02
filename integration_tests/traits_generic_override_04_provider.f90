! The provider of traits_generic_override_04: a protocol, its intrinsic
! conformances, a nonsealed type with open slots and its extension, and a
! generic type, compiled separately from their clients.
module traits_generic_override_04_traits
    implicit none
    private
    public :: INumeric, ISum

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself) :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum
end module traits_generic_override_04_traits

module traits_generic_override_04_intrinsics
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_override_04_traits, only: INumeric

    implements INumeric :: integer
    end implements integer

    implements INumeric :: real(real64)
    end implements real(real64)
end module traits_generic_override_04_intrinsics

module traits_generic_override_04_base
    use traits_generic_override_04_traits, only: INumeric, ISum
    implicit none
    private
    public :: Base, Tenfold, total

    type, implements(ISum) :: Base
    contains
        procedure :: sum => base_sum
        procedure :: twice
        procedure :: largest
    end type Base

    type, extends(Base) :: Tenfold
    contains
        procedure :: sum => tenfold_sum
    end type Tenfold

contains

    function base_sum{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function base_sum

    ! Inherited by Tenfold: its calls of `sum` dispatch.
    function twice{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%sum(x) + self%sum(x)
    end function twice

    ! Its local array cannot be erased, so it has no open slot and is
    ! reached by static calls only.
    function largest{INumeric :: T}(self, x) result(n)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        integer :: n
        type(T) :: copy(size(x))
        copy = x
        n = size(copy)
    end function largest

    function tenfold_sum{INumeric :: T}(self, x) result(s)
        class(Tenfold), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(0)
        do i := 1, 10
            s = s + self%Base%sum(x)
        end do
    end function tenfold_sum

    ! Forwards its own type parameter to a dispatching call.
    function total{INumeric :: T}(object, x) result(s)
        class(Base), intent(in) :: object
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = object%sum(x)
    end function total
end module traits_generic_override_04_base

module traits_generic_override_04_simple
    use traits_generic_override_04_traits, only: INumeric, ISum
    implicit none
    private
    public :: Simple

    type, implements(ISum) :: Simple{ISum :: U}
        type(U), allocatable :: inner
    contains
        initial :: init
        procedure :: sum
        procedure :: twice
    end type Simple

contains

    function init(inner) result(res)
        type(U), intent(in) :: inner
        type(Simple{U}) :: res
        res%inner = inner
    end function init

    function sum{INumeric :: T}(self, x) result(s)
        class(Simple{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%inner%sum(x)
    end function sum

    function twice{INumeric :: T}(self, x) result(s)
        class(Simple{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%sum(x) + self%sum(x)
    end function twice
end module traits_generic_override_04_simple

