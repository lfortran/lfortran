module separate_compilation_77a
    implicit none
    integer :: counter = 0
    type :: shape
        integer :: n = 0
    contains
        procedure :: area => shape_area
    end type
contains
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
        counter = counter + 1
    end function

    integer function shape_area(self)
        class(shape), intent(in) :: self
        shape_area = self%n*self%n
    end function
end module
