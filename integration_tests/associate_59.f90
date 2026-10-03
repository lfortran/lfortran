! ASSOCIATE with a parenthesized array selector.
!
! A parenthesized selector `(x)` is a primary (R1001), not a designator, so per
! F2018 11.1.3.3 the selector is an expression: it is evaluated when the
! ASSOCIATE statement executes and the associate name holds a copy of its value.
! The block below therefore swaps a and b.
!
! This test is not labelled `gfortran`: GFortran 16.1 aliases the associate
! name to the selector at every optimization level and prints
! "a = 4 5 6, b = 4 5 6". The scalar cases, which GFortran gets right, are in
! associate_58. See https://github.com/lfortran/lfortran/issues/12611.
program associate_59
    implicit none
    integer :: a(3), b(3)
    real :: r(2)

    a = [1, 2, 3]
    b = [4, 5, 6]
    associate (tmp => (a))
        a = b
        b = tmp
    end associate
    if (any(a /= [4, 5, 6])) error stop 1
    if (any(b /= [1, 2, 3])) error stop 2

    ! The copy has the shape of the selector and is independent of it
    r = [1.0, 2.0]
    associate (p => (r))
        if (size(p) /= 2) error stop 3
        r = [3.0, 4.0]
        if (abs(p(1) - 1.0) > 1.0e-6) error stop 4
        if (abs(p(2) - 2.0) > 1.0e-6) error stop 5
    end associate

    ! Without the parentheses the selector is a variable, so the associate name
    ! must stay associated with its storage and see the redefinition
    a = [1, 2, 3]
    b = [4, 5, 6]
    associate (tmp => a)
        a = b
        if (any(tmp /= [4, 5, 6])) error stop 6
    end associate

    print *, "ok"
end program associate_59
