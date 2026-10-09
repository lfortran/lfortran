module traits_inheritance_03_m
    implicit none

    abstract interface :: IBase
        function value() result(res)
            integer :: res
        end function value
    end interface IBase

    abstract interface, extends(IBase) :: ILeft
        function left_value() result(res)
            integer :: res
        end function left_value
    end interface ILeft

    abstract interface, extends(IBase) :: IRight
        function right_value() result(res)
            integer :: res
        end function right_value
    end interface IRight

    abstract interface, extends(ILeft + IRight) :: IDiamond
    end interface IDiamond

    type :: Payload
        integer :: data
    end type Payload

    implements IDiamond :: Payload
        procedure, pass :: right_value => payload_right
        procedure, pass :: value => payload_value
        procedure, pass :: left_value => payload_left
    end implements Payload

contains

    function payload_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function payload_left(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = 2 * self%data
    end function payload_left

    function payload_right(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = 3 * self%data
    end function payload_right

    function base_value{IBase :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function base_value

    function left_total{ILeft :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = base_value(object) + object%left_value()
    end function left_total

    function right_total{IRight :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = base_value(object) + object%right_value()
    end function right_total

    function diamond_total{IDiamond :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = left_total(object) + right_total(object) + object%value()
    end function diamond_total

    function redundant_total{IDiamond + ILeft + IBase + IRight :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = diamond_total(object) + base_value(object)
    end function redundant_total
end module traits_inheritance_03_m

program traits_inheritance_03
    use traits_inheritance_03_m
    implicit none
    type(Payload) :: object

    object = Payload(5)
    if (base_value(object) /= 5) error stop 1
    if (left_total(object) /= 15) error stop 2
    if (right_total(object) /= 20) error stop 3
    if (diamond_total(object) /= 40) error stop 4
    if (redundant_total(object) /= 45) error stop 5
    if (redundant_total{Payload}(object) /= 45) error stop 6
end program traits_inheritance_03
