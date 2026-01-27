! Test implicit interface procedure dummy with mixed call patterns
! When a procedure dummy f is:
!   1. Passed as argument to another subroutine
!   2. Called with both variable args f(x) and expression args f(x+1)
! All calls should use consistent by-reference calling convention
program implicit_interface_23
    implicit none
    double precision :: x, result
    x = 2.0d0
    call compute(square, x, result)
    ! Expected: square(2) + square(3) = 4 + 9 = 13
    if (abs(result - 13.0d0) > 1.0d-10) error stop
    print *, "PASS"
contains
    double precision function square(y)
        double precision, intent(in) :: y
        square = y * y
    end function
end program

subroutine compute(f, x, result)
    implicit none
    double precision, intent(in) :: x
    double precision, intent(out) :: result
    double precision :: f, tmp
    external :: f
    ! Pass f to another subroutine first (creates implicit interface)
    tmp = x
    call use_func(f, tmp)
    ! Then call f with both variable and expression arguments
    result = f(x) + f(x + 1.0d0)
end subroutine

subroutine use_func(g, z)
    implicit none
    double precision, intent(inout) :: z
    double precision :: g
    external :: g
    z = g(z)
end subroutine
