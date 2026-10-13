! Main-program types adopt a closed numeric generic message, called statically
! or never used, and an open-world generic message called statically. A program
! cannot own the provider entries of generic runtime slots, so these
! conformances stay static-only, while the contracts without generic messages,
! including a concrete array and real-result message, keep their runtime views.
! GFortran does not accept this LFortran extension.
module traits_generic_binding_02_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IValue, IApply, IMean

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IValue
        integer function value()
        end function value
    end interface IValue

    abstract interface :: IApply
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer             :: r
        end function apply
    end interface IApply

    abstract interface :: IMean
        function mean(x) result(m)
            import :: real64
            real(real64), intent(in) :: x(:)
            real(real64)             :: m
        end function mean
    end interface IMean
end module traits_generic_binding_02_m

program traits_generic_binding_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_02_m
    implicit none

    type, sealed, implements(ISum + IValue) :: Adder
        integer :: bias = 0
    contains
        procedure, pass(self) :: sum => adder_sum
        procedure, nopass :: value => adder_value
    end type Adder

    type, sealed, implements(ISum) :: Unused
    contains
        procedure, nopass :: sum => unused_sum
    end type Unused

    type :: Offset
        integer :: amount = 0
    end type Offset

    implements IApply :: Offset
        procedure, pass(self) :: apply => offset_apply
    end implements

    type, sealed, implements(IMean) :: Averager
    contains
        procedure, nopass :: mean => averager_mean
    end type Averager

    type(Adder)                :: a
    type(Offset)               :: o
    type(Averager)             :: avg
    class(IValue), allocatable :: item
    class(IMean), allocatable  :: averaging
    integer                    :: xi(5) = [1, 2, 3, 4, 5]
    real(real64)               :: xr(4) = [1.0_real64, 2.0_real64, 3.0_real64, 6.0_real64]

    a%bias = 10
    if (a%sum(xi) /= 25) error stop 1
    if (a%sum(xi(5:1:-2)) /= 19) error stop 2
    if (a%sum(xr) /= 22.0_real64) error stop 3
    if (a%sum{real(real64)}(xr(2:3)) /= 15.0_real64) error stop 4
    o%amount = 5
    if (o%apply(a) /= 12) error stop 5
    if (avg%mean(xr) /= 3.0_real64) error stop 6
    allocate(item, source=a)
    if (item%value() /= 7) error stop 7
    allocate(averaging, source=avg)
    if (averaging%mean(xr(1:2)) /= 1.5_real64) error stop 8
    deallocate(item, averaging)
    print '(a)', "program adopters: static generic calls and concrete views"

contains

    function adder_sum{INumeric :: T}(x, self) result(s)
        type(T), intent(in)     :: x(:)
        type(Adder), intent(in) :: self
        type(T)                 :: s
        integer                 :: i
        s = T(self%bias)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function adder_sum

    integer function adder_value()
        adder_value = 7
    end function adder_value

    function unused_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = T(size(x))
    end function unused_sum

    function offset_apply{IValue :: E}(object, self) result(r)
        type(E), intent(in)       :: object
        class(Offset), intent(in) :: self
        integer                   :: r
        r = object%value() + self%amount
    end function offset_apply

    function averager_mean(x) result(average)
        real(real64), intent(in) :: x(:)
        real(real64)             :: average
        average = sum(x) / size(x)
    end function averager_mean
end program traits_generic_binding_02
