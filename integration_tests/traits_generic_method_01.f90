module traits_generic_method_01_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    abstract interface :: ILeft
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    abstract interface :: IRight
        function apply{IValue :: Renamed}(object) result(r)
            type(Renamed), intent(in) :: object
            integer :: r
        end function
    end interface
    abstract interface, extends(ILeft + IRight) :: IAlgorithm
    end interface
    type :: Value
        integer :: payload
    end type
    type :: Offset
        integer :: amount
    end type
    type :: Scaled
    end type
    implements IValue :: Value
        procedure, pass :: value => read_value
    end implements
    implements IAlgorithm :: Offset
        procedure, pass(self) :: apply => offset_apply
    end implements
    implements IAlgorithm :: Scaled
        procedure, nopass :: apply => scaled_apply
    end implements
contains
    integer function read_value(self) result(r)
        class(Value), intent(in) :: self
        r = self%payload
    end function
    function offset_apply{IValue :: Element}(object, self) result(r)
        type(Element), intent(in) :: object
        class(Offset), intent(in) :: self
        integer :: r
        r = object%value() + self%amount
    end function
    function scaled_apply{IValue :: Item}(object) result(r)
        type(Item), intent(in) :: object
        integer :: r
        r = 2*object%value() + 100
    end function
end module

program traits_generic_method_01
    use traits_generic_method_01_m
    implicit none
    type(Value) :: object
    type(Offset) :: first
    type(Scaled) :: second
    object = Value(37)
    first = Offset(10)
    if (first%apply(object) /= 47) error stop 1
    if (first%apply{Value}(object) /= 47) error stop 2
    if (second%apply(object) /= 174) error stop 3
    if (second%apply{Value}(object) /= 174) error stop 4
    if (offset_apply(object, first) /= 47) error stop 5
    if (scaled_apply{Value}(object) /= 174) error stop 6
end program
