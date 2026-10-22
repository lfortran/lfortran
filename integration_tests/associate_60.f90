! Associate names of overlapping sections of one array: the expression of
! an assignment is evaluated before any part of the variable is defined
! (gfortran skips the temporary in these cases, so the test is llvm only)
program associate_60
    implicit none
    type :: tt
        real :: arr(6)
    end type
    integer :: i
    real :: a(6), a0(6), c(6,2), c0(6,2)
    type(tt) :: t

    a0 = [(real(i), i = 1, 6)]
    c0(:,1) = a0
    c0(:,2) = -a0

    ! Centred difference through three associate names
    a = a0
    associate (an => a(2:5), ae => a(3:6), aw => a(1:4))
        an = ae - aw
    end associate
    if (any(a /= [1., 2., 2., 2., 2., 6.])) error stop 1

    ! Shift right
    a = a0
    associate (lhs => a(2:6), rhs => a(1:5))
        lhs = rhs
    end associate
    if (any(a /= [1., 1., 2., 3., 4., 5.])) error stop 2

    ! Shift left
    a = a0
    associate (lhs => a(1:5), rhs => a(2:6))
        lhs = rhs
    end associate
    if (any(a /= [2., 3., 4., 5., 6., 6.])) error stop 3

    ! An associate name and its own variable
    a = a0
    associate (lhs => a(2:6))
        lhs = a(1:5)
    end associate
    if (any(a /= [1., 1., 2., 3., 4., 5.])) error stop 4

    ! Associate names of an associate name
    a = a0
    associate (b => a)
        associate (lhs => b(2:6), rhs => b(1:5))
            lhs = rhs
        end associate
    end associate
    if (any(a /= [1., 1., 2., 3., 4., 5.])) error stop 5

    ! 2-D sections, literal column
    c = c0
    associate (cn => c(2:5,1), ce => c(3:6,1), cw => c(1:4,1))
        cn = ce - cw
    end associate
    if (any(c(:,1) /= [1., 2., 2., 2., 2., 6.])) error stop 6
    if (any(c(:,2) /= c0(:,2))) error stop 7

    ! 2-D sections, run-time column
    c = c0
    call centred(c, 2, 2)
    if (any(c(:,2) /= [-1., -2., -2., -2., -2., -6.])) error stop 8
    if (any(c(:,1) /= c0(:,1))) error stop 9

    ! A whole fixed-size array and its own variable
    a = a0
    associate (b => a)
        b(2:6) = a(1:5)
    end associate
    if (any(a /= [1., 1., 2., 3., 4., 5.])) error stop 10

    a = a0
    associate (b => a)
        a(2:6) = b(1:5)
    end associate
    if (any(a /= [1., 1., 2., 3., 4., 5.])) error stop 11

    ! A whole fixed-size component and the component itself
    t%arr = a0
    associate (b => t%arr)
        b(2:6) = t%arr(1:5)
    end associate
    if (any(t%arr /= [1., 1., 2., 3., 4., 5.])) error stop 12

    t%arr = a0
    associate (b => t%arr)
        t%arr(2:6) = b(1:5)
    end associate
    if (any(t%arr /= [1., 1., 2., 3., 4., 5.])) error stop 13

    ! A whole explicit-shape dummy and the dummy itself
    a = a0
    call shift_explicit(a, size(a))
    if (any(a /= [1., 1., 2., 3., 4., 5.])) error stop 14

    ! A column of an explicit-shape dummy and the dummy itself
    c = c0
    call shift_column(c, size(c, 1))
    if (any(c(:,1) /= [1., 1., 2., 3., 4., 5.])) error stop 15
    if (any(c(:,2) /= c0(:,2))) error stop 16

    print *, "ok"

contains

    subroutine shift_explicit(x, n)
        integer, intent(in) :: n
        real, intent(inout) :: x(n)
        associate (b => x)
            x(2:n) = b(1:n-1)
        end associate
    end subroutine

    subroutine shift_column(x, n)
        integer, intent(in) :: n
        real, intent(inout) :: x(n,2)
        associate (b => x(:,1))
            b(2:n) = x(1:n-1,1)
        end associate
    end subroutine

    subroutine centred(x, new, mid)
        real, intent(inout) :: x(:,:)
        integer, intent(in) :: new, mid
        integer :: n
        n = size(x, 1)
        associate (xn => x(2:n-1,new), xe => x(3:n,mid), xw => x(1:n-2,mid))
            xn = xe - xw
        end associate
    end subroutine

end program
