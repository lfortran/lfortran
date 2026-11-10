! The signature of a procedure is built before its local variables. Its
! dummy arguments and result may use local named constants, interfaces and
! COMMON objects.
module spec_expr_12_mod
    implicit none
    integer, parameter :: dp = kind(1.0)
contains
    ! The local `dp` hides the module's `dp`.
    function twice(x) result(r)
        integer, parameter :: dp = kind(1.0d0)
        real(dp), intent(in) :: x
        real(dp) :: r
        real(dp) :: unused
        unused = 0
        r = 2*x
    end function

    subroutine fill(a)
        integer :: n
        parameter (n = 3)
        integer, intent(out) :: a(n)
        a = n
    end subroutine

    subroutine call_it(f, y)
        abstract interface
            real function f_t(x)
                real, intent(in) :: x
            end function
        end interface
        procedure(f_t) :: f
        real, intent(out) :: y
        y = f(2.0)
    end subroutine

    real function square(x)
        real, intent(in) :: x
        square = x*x
    end function
end module

subroutine spec_expr_12_common(a)
    implicit none
    integer :: m
    common /spec_expr_12_blk/ m
    integer, intent(out) :: a(m)
    a = m
end subroutine

program spec_expr_12
    use spec_expr_12_mod
    implicit none
    integer :: a(3), b(4), m
    common /spec_expr_12_blk/ m
    real :: y
    if (kind(twice(1.0d0)) /= 8) error stop
    if (abs(twice(1.5d0) - 3.0d0) > 1d-12) error stop
    call fill(a)
    if (any(a /= 3)) error stop
    call call_it(square, y)
    if (abs(y - 4.0) > 1e-6) error stop
    m = 4
    call spec_expr_12_common(b)
    if (any(b /= 4)) error stop
    print *, "ok"
end program
