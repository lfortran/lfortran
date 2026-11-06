module pass_array_by_data_17_module
    implicit none
    type :: accumulator
        real :: total
    contains
        procedure :: add
        procedure :: add_matrix
    end type
contains
    subroutine add(self, x, lower)
        class(accumulator), intent(inout) :: self
        integer, intent(in) :: lower
        real, intent(in) :: x(lower:)
        call calculate(x, self%total)
    end subroutine

    subroutine calculate(x, total)
        real, intent(in) :: x(:)
        real, intent(out) :: total
        total = sum(x)
    end subroutine

    subroutine add_matrix(self, x)
        class(accumulator), intent(inout) :: self
        real, intent(in) :: x(4:,-2:)
        call calculate_matrix(x, self%total)
    end subroutine

    subroutine calculate_matrix(x, total)
        real, intent(in) :: x(:,:)
        real, intent(out) :: total
        total = sum(x)
    end subroutine
end module
