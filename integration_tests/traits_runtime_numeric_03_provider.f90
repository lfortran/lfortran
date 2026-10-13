! Provider for traits_runtime_numeric_03.f90, compiled separately. Its type
! names sort before their generic bindings, so a client loading this module
! resolves each generic type-bound binding through the module's import.
module traits_runtime_numeric_03_provider_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IPair, IShape, Adder, Pairing, Shape, IValue, &
        Cell, observe, show, make_cell

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    ! Two closed binders: one member slot per member pair.
    abstract interface :: IPair
        function weigh{INumeric :: T, INumeric :: U}(x, w) result(s)
            type(T), intent(in) :: x(:)
            type(U), intent(in) :: w
            type(T)             :: s
        end function weigh
    end interface IPair

    abstract interface :: IShape
        function cells(x) result(n)
            complex(real64), intent(in) :: x(:, :)
            integer                     :: n
        end function cells
        function positive(x) result(p)
            real(real64), intent(in) :: x(:)
            logical                  :: p
        end function positive
        function mean(x) result(m)
            integer, intent(in) :: x(0:)
            real(real64)        :: m
        end function mean
    end interface IShape

    abstract interface :: IValue
        integer function value()
        end function value
    end interface IValue

    type, sealed, implements(ISum) :: Adder
    contains
        procedure, nopass :: sum
    end type Adder

    type, sealed, implements(IPair) :: Pairing
        integer :: bias = 0
    contains
        procedure, pass(self) :: weigh
    end type Pairing

    type, sealed, implements(IShape) :: Shape
    contains
        procedure, nopass :: cells
        procedure, nopass :: positive
        procedure, nopass :: mean
    end type Shape

    type, sealed, implements(IValue) :: Cell
        integer :: n = 0
    contains
        procedure, nopass :: value => cell_value
    end type Cell

    interface observe
        module procedure observe_value
    end interface observe

    interface show
        module procedure show_value
    end interface show

contains

    function sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function sum

    function weigh{INumeric :: T, INumeric :: U}(x, w, self) result(s)
        type(T),       intent(in) :: x(:)
        type(U),       intent(in) :: w
        type(Pairing), intent(in) :: self
        type(T)                   :: s
        integer                   :: i
        s = T(self%bias)
        do i = 1, size(x)
            s = s + x(i)
        end do
        if (w > U(0)) s = s * T(2)
    end function weigh

    function cells(x) result(n)
        complex(real64), intent(in) :: x(:, :)
        integer                     :: n
        n = size(x, 1) * 100 + size(x, 2)
    end function cells

    function positive(x) result(p)
        real(real64), intent(in) :: x(:)
        logical                  :: p
        p = all(x > 0)
    end function positive

    function mean(x) result(m)
        integer, intent(in) :: x(0:)
        real(real64)        :: m
        m = real(x(0) + x(ubound(x, 1)), real64) / 2
    end function mean

    integer function cell_value()
        cell_value = 7
    end function cell_value

    integer function observe_value(item)
        class(IValue), intent(in) :: item
        observe_value = item%value()
    end function observe_value

    subroutine show_value(item, n)
        class(IValue), intent(in)  :: item
        integer,       intent(out) :: n
        n = item%value() + 1
    end subroutine show_value

    function make_cell(n) result(object)
        integer, intent(in) :: n
        type(Cell)          :: object
        object%n = n
    end function make_cell
end module traits_runtime_numeric_03_provider_m
