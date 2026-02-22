module class_110_module
    implicit none

    type :: base_type
        integer :: k
    contains
        procedure :: nelements
    end type base_type

    type :: wrapper
        class(base_type), allocatable :: obj
    contains
        procedure :: caller
    end type wrapper

contains

    pure function nelements(self, i) result(n)
        class(base_type), intent(in) :: self
        integer, intent(in) :: i
        integer :: n
        n = self%k
    end function nelements

    subroutine caller(self)
        class(wrapper), intent(in) :: self
        real(8), dimension(self%obj%nelements(20)) :: a
        if (size(a) /= self%obj%k) error stop 1
    end subroutine caller

end module class_110_module
