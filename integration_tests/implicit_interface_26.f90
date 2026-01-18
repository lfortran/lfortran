! Test that visit order does not affect type propagation for implicit interfaces.
! dqc25c is defined BEFORE dqk15w - exercises reverse type propagation.

program implicit_interface_26
    implicit none
    double precision :: result

    call dqc25c(square, 0.0d0, 1.0d0, result)
    if (abs(result - 0.125d0) > 1.0d-10) error stop
    print *, "PASSED"

contains
    double precision function square(x)
        double precision, intent(in) :: x
        square = x * x
    end function
end program

double precision function dqwgtc(x)
    double precision, intent(in) :: x
    dqwgtc = 0.5d0
end function

! Caller - does NOT call f or dqwgtc, only passes them (defined BEFORE dqk15w)
subroutine dqc25c(f, a, b, result)
    double precision, external :: f
    double precision, external :: dqwgtc
    double precision, intent(in) :: a, b
    double precision, intent(out) :: result
    call dqk15w(f, dqwgtc, a, b, result)
end subroutine

! Callee - actually calls f and w (defined AFTER dqc25c)
subroutine dqk15w(f, w, a, b, result)
    double precision, external :: f
    double precision, external :: w
    double precision, intent(in) :: a, b
    double precision, intent(out) :: result
    double precision :: centr
    centr = 0.5d0*(a+b)
    result = f(centr) * w(centr)
end subroutine
