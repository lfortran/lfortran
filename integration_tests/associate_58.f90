! ASSOCIATE with a parenthesized selector.
!
! A parenthesized selector `(x)` is a primary (R1001), not a designator, so per
! F2018 11.1.3.3 the selector is an expression: it is evaluated when the
! ASSOCIATE statement executes and the associate name holds a copy of its value.
! Redefining `x` inside the block must not be visible through the associate
! name. Without the parentheses the selector is a variable and the associate
! name is associated with its storage, so the redefinition *is* visible.
!
! This file only covers scalar selectors, which GFortran gets right. The array
! case lives in associate_59, which cannot be labelled `gfortran`.
program associate_58
    implicit none
    integer :: a, b
    real :: r
    logical :: l
    character(len=3) :: c

    ! Parenthesized integer scalar: the associate name is a copy
    a = 1
    b = 2
    associate (p => (a))
        a = b
        if (p /= 1) error stop 1
    end associate
    if (a /= 2) error stop 2

    ! Nested parentheses are still just an expression
    a = 1
    associate (p => ((a)))
        a = 9
        if (p /= 1) error stop 3
    end associate

    ! Real scalar
    r = 1.5
    associate (p => (r))
        r = 2.5
        if (abs(p - 1.5) > 1.0e-6) error stop 4
    end associate

    ! Logical scalar
    l = .true.
    associate (p => (l))
        l = .false.
        if (.not. p) error stop 5
    end associate

    ! Character scalar
    c = "abc"
    associate (p => (c))
        c = "xyz"
        if (p /= "abc") error stop 6
        if (len(p) /= 3) error stop 7
    end associate

    ! Without the parentheses the selector is a variable, so the associate name
    ! must stay associated with its storage and see the redefinition
    a = 1
    b = 2
    associate (p => a)
        a = b
        if (p /= 2) error stop 8
    end associate

    print *, "ok"
end program associate_58
