! The provider: conforming types, one of them private, with FINAL procedures.
module traits_runtime_constructor_component_separate_01_provider_m
    use traits_runtime_constructor_component_separate_01_contracts_m, only: &
        IShape, IName, Holder
    implicit none
    private
    public :: Circle, make_square, finals, circle_holder
    integer :: finals = 0
    type :: Circle
        real :: r = 1
    contains
        final :: finish_circle
    end type
    type :: Square
        real :: side = 1
        character(len=:), allocatable :: label
    contains
        final :: finish_square
    end type
    implements IShape :: Circle
        procedure :: area => circle_area
        procedure :: scale => circle_scale
    end implements
    implements IShape :: Square
        procedure :: area => square_area
        procedure :: scale => square_scale
    end implements
    implements IName :: Square
        procedure :: name => square_name
    end implements
contains
    real function circle_area(self)
        type(Circle), intent(in) :: self
        circle_area = 3 * self%r**2
    end function
    subroutine circle_scale(self, f)
        type(Circle), intent(inout) :: self
        real, intent(in) :: f
        self%r = self%r * f
    end subroutine
    real function square_area(self)
        type(Square), intent(in) :: self
        square_area = self%side**2
    end function
    subroutine square_scale(self, f)
        type(Square), intent(inout) :: self
        real, intent(in) :: f
        self%side = self%side * f
    end subroutine
    function square_name(self) result(res)
        type(Square), intent(in) :: self
        character(len=:), allocatable :: res
        res = self%label
    end function
    subroutine finish_circle(self)
        type(Circle), intent(inout) :: self
        finals = finals + 1
    end subroutine
    subroutine finish_square(self)
        type(Square), intent(inout) :: self
        finals = finals + 1
    end subroutine
    function make_square(side, label) result(res)
        real, intent(in) :: side
        character(*), intent(in) :: label
        class(IShape+IName), allocatable :: res
        type(Square) :: s
        s%side = side
        s%label = label
        res = s
    end function
    ! A default constructor given a value of a private type.
    function circle_holder(r) result(res)
        real, intent(in) :: r
        type(Holder) :: res
        res = Holder(Circle(r), 1)
    end function
end module
