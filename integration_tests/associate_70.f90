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
    integer, allocatable :: m2(:, :)
    real, pointer :: q(:)
    real, parameter :: p(0:2) = [1.0, 2.0, 3.0]
    type(t) :: tt
    type(t), parameter :: pt = t([4, 5, 6])
    type(t) :: ts(0:1)
    real, target :: a2(0:1)

    a = [1.0, 2.0, 3.0, 4.0, 5.0]
    m = 3.0
    allocate(b(0:4))
    allocate(m2(0:1, -1:1))
    m2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
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
        if (abs(sum(x) - 15.0) > 1e-6) error stop
    end associate
    associate (x => b + 1.0)
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(x(5) - 6.0) > 1e-6) error stop
    end associate
    associate (x => (q))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (abs(sum(x) - 15.0) > 1e-6) error stop
    end associate
    associate (x => (m2))
        if (any(lbound(x) /= [1, 1]) .or. any(ubound(x) /= [2, 3])) error stop
        if (sum(x) /= 21) error stop
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

    ! A component of a named constant has the bounds that LBOUND and UBOUND
    ! give for it.
    associate (x => pt%v)
        if (lbound(x, 1) /= lbound(pt%v, 1)) error stop
        if (ubound(x, 1) /= ubound(pt%v, 1)) error stop
        if (sum(x) /= 15) error stop
    end associate
    ts(0)%v = [1, 2, 3]
    ts(1)%v = [4, 5, 6]
    associate (x => ts(1)%v)
        if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 2) error stop
        if (x(0) /= 4) error stop
    end associate

    ! The shape of the associate name is that of the selector when the
    ! construct is entered.
    associate (x => (b))
        deallocate(b)
        allocate(b(0:7))
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (size(x) /= 5) error stop
    end associate
    a2 = 0.0
    associate (x => (q))
        q(0:) => a2
        if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
        if (size(x) /= 5) error stop
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
            if (abs(sum(x) - 15.0) > 1e-6) error stop
        end associate
        associate (x => d + 1.0, y => (d))
            if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 5) error stop
            if (lbound(y, 1) /= 1 .or. ubound(y, 1) /= 5) error stop
            if (abs(x(5) - 6.0) > 1e-6) error stop
        end associate
    end subroutine

end program
