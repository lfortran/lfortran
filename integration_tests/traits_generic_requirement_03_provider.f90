! Conformances of traits_generic_requirement_03, compiled separately from the
! generic procedures that use them.
module traits_generic_requirement_03_provider
    use traits_generic_requirement_03_contracts, only: INumeric, ISum, IValue, IApply
    implicit none
    private
    public :: SimpleSum, ScaledSum, Value, Offset

    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, pass :: sum => simple_sum
    end type SimpleSum

    type, sealed, implements(ISum) :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass(self) :: sum => scaled_sum
    end type ScaledSum

    type :: Value
        integer :: payload = 0
    end type Value

    type :: Offset
        integer :: amount = 0
    end type Offset

    implements IValue :: Value
        procedure, pass :: value => read_value
    end implements

    implements IApply :: Offset
        procedure, pass(self) :: apply => offset_apply
    end implements

contains

    function simple_sum{INumeric :: T}(self, x) result(s)
        type(SimpleSum), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum

    function scaled_sum{INumeric :: T}(x, self) result(s)
        type(T),         intent(in) :: x(:)
        type(ScaledSum), intent(in) :: self
        type(T)                     :: s
        s = T(self%factor) * simple_sum(SimpleSum(), x)
    end function scaled_sum

    integer function read_value(self) result(r)
        class(Value), intent(in) :: self
        r = self%payload
    end function read_value

    function offset_apply{IValue :: Element}(object, self) result(r)
        type(Element), intent(in) :: object
        class(Offset), intent(in) :: self
        integer :: r
        r = object%value() + self%amount
    end function offset_apply
end module traits_generic_requirement_03_provider
