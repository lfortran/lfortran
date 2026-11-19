program associate_70
    ! The lower bound of an associate name is LBOUND of the selector, which
    ! is 1 in every dimension unless the selector is a whole array.
    implicit none
    type :: t
        integer :: v(0:2)
    end type
    real, target :: a(0:4)
    real :: m(0:1, -1:1)
    real, allocatable :: b(:)
    real, pointer :: q(:)
    real, parameter :: p(0:2) = [1.0, 2.0, 3.0]
    type(t) :: tt

    a = [1.0, 2.0, 3.0, 4.0, 5.0]
    m = 3.0
    allocate(b(0:4))
    b = a
    q(0:) => a
    tt%v = [7, 8, 9]

    associate (x => a)
        if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 4) error stop
    end associate
    associate (x => a(:))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
    end associate
    associate (x => (a))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (size(x) /= 5) error stop
    end associate
    associate (x => ((a)))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(sum(x) - 15.0) > 1e-6) error stop
    end associate
    associate (x => a + 1.0)
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(x(1) - 2.0) > 1e-6 .or. abs(x(5) - 6.0) > 1e-6) error stop
    end associate
    associate (x => -a)
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(x(5) + 5.0) > 1e-6) error stop
    end associate
    associate (x => abs(a))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
    end associate
    associate (x => (m))
        if (any(lbound(x) /= [1, 1]) .or. any(ubound(x) /= [2, 3])) error stop
        if (abs(sum(x) - 18.0) > 1e-6) error stop
    end associate

    associate (x => p)
        if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 2) error stop
    end associate
    associate (x => (p))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 3) error stop
    end associate

    associate (x => b)
        if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 4) error stop
    end associate
    associate (x => (b))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
    end associate
    associate (x => b + 1.0)
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(x(5) - 6.0) > 1e-6) error stop
    end associate
    associate (x => (q))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
    end associate
    associate (x => q + 1.0)
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(x(1) - 2.0) > 1e-6) error stop
    end associate

    associate (x => tt%v)
        if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 2) error stop
    end associate
    associate (x => (tt%v))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 3) error stop
    end associate
    associate (x => tt%v + 1)
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 3) error stop
        if (x(3) /= 10) error stop
    end associate

    call sub(a, a, 3)
    print *, "ok"

contains

    subroutine sub(c, d, n)
        integer, intent(in) :: n
        real, intent(in) :: c(0:n + 1)
        real, intent(in) :: d(0:)
        associate (x => (c))
            if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        end associate
        associate (x => c * 2.0)
            if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
            if (abs(x(1) - 2.0) > 1e-6 .or. abs(x(5) - 10.0) > 1e-6) error stop
        end associate
        associate (x => (d))
            if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        end associate
        associate (x => d + 1.0, y => (d))
            if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
            if (lbound(y, 1) /= 1 .or. ubound(y, 1) /= 5) error stop
            if (abs(x(5) - 6.0) > 1e-6) error stop
        end associate
    end subroutine

end program
