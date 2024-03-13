module Geometry
    implicit none

    type :: Circle
        real :: radius
contains
    procedure :: calculateArea
    end type Circle

contains

    ! Type-bound subroutine to calculate the area of a circle
    subroutine calculateArea(self, area)
       class(Circle), intent(in) :: self
       real, intent(out) :: area
       area = 3.14 * self%radius**2
    end subroutine calculateArea
end module Geometry


program TestGeometry
    use Geometry
    implicit none

    type(Circle) :: myCircle
    real :: circleArea

    myCircle%radius = 5.0

    call myCircle%calculateArea(circleArea, 12)

    print *, "Circle Area:", circleArea
end program TestGeometry
