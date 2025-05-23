module shapes_module
    implicit none

    type, abstract :: Shape
    contains
        procedure(area_interface), deferred :: area
    end type Shape

    abstract interface
        function area_interface(self) result(res)
            import :: Shape
            class(Shape), intent(in) :: self
            real :: res
        end function area_interface
    end interface

    type, extends(Shape) :: Circle
        real :: radius
    contains
        procedure :: area => circle_area
    end type Circle

contains

    function circle_area(self) result(res)
        class(Circle), intent(in) :: self
        real :: res
        res = 3.14159 * self%radius**2
    end function circle_area

end module shapes_module


program main
    use shapes_module
    implicit none

    class(Shape), allocatable :: s
    real :: a

    allocate(Circle :: s)

end program main
