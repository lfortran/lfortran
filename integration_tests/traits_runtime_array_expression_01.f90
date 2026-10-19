! A runtime message with an array argument and a scalar result is an operand of
! array expressions: the argument is passed whole, not one element per
! element of the expression.
module traits_runtime_array_expression_01_m
    implicit none
    abstract interface :: ITotal
        function total(self, z) result(r)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: z(:)
            real(8) :: r
        end function total
    end interface
    type :: Summer
        real(8) :: offset = 0
    end type Summer
    implements ITotal :: Summer
        procedure, pass :: total => summer_total
    end implements
contains
    function summer_total(self, z) result(r)
        class(Summer), intent(in) :: self
        real(8), intent(in) :: z(:)
        real(8) :: r
        r = sum(z) + self%offset
    end function summer_total

    subroutine check(o)
        class(ITotal), intent(in) :: o
        real(8) :: x(3), y(3), z(3)
        x = [1d0, 2d0, 3d0]
        z = [1d0, 2d0, 3d0]
        y = x * o%total(z)
        if (any(y /= [6, 12, 18])) error stop 1
        y = o%total(z) + 0 * x
        if (any(y /= 6)) error stop 2
        y = o%total(z(1:2)) - x
        if (any(y /= [2, 1, 0])) error stop 3
        where (x > 1) y = o%total(x)
        if (any(y /= [2, 6, 6])) error stop 4
    end subroutine check
end module traits_runtime_array_expression_01_m

program traits_runtime_array_expression_01
    use traits_runtime_array_expression_01_m
    implicit none
    class(ITotal), allocatable :: owner
    call check(Summer())
    owner = Summer(0)
    call check(owner)
    print '(a)', 'traits_runtime_array_expression_01 ok'
end program traits_runtime_array_expression_01
