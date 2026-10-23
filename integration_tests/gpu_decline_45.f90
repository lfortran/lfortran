! gpu decline reason: LiveoutOptionalPassedOn
! An optional dummy argument the loop may define, whose value is kept after
! it, that the loop passes on to an optional dummy argument of a type-bound
! function, whose passed object is not among the actual arguments: each
! thread would pass its private copy, which is present whether or not the
! argument is.
module gpu_decline_45_m
    implicit none
    type :: op_t
        real :: c = 2
    contains
        procedure :: g
    end type
contains
    pure real function g(self, x, k)
        class(op_t), intent(in) :: self
        real, intent(in) :: x
        integer, intent(in), optional :: k
        if (present(k)) then
            g = self%c * x + 1000
        else
            g = self%c * x
        end if
    end function
    subroutine s(x, y, k)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer, intent(inout), optional :: k
        type(op_t) :: o
        integer :: i
        do concurrent (i = 1:size(x))
            if (present(k)) then
                if (i == 5) k = i
            end if
            y(i) = o%g(x(i), k)
        end do
    end subroutine
end module
program gpu_decline_45
    use gpu_decline_45_m
    implicit none
    real :: x(100), y(100)
    integer :: i, k
    do i = 1, 100
        x(i) = i
    end do
    call s(x, y)
    print *, y(7)
    if (y(7) /= 14) error stop 1
end program
