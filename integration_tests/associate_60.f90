! ASSOCIATE with a parenthesized array element or a component of an array
! element as the selector. Like `(x)` in associate_58, `(a(2))` is an
! expression, so the associate name holds a copy of its value.
program associate_60
    implicit none
    type :: t
        integer :: c
    end type
    integer :: a(3)
    type(t) :: d(2)

    a = 1
    associate (p => (a(2)))
        a(2) = 5
        if (p /= 1) error stop 1
    end associate
    if (a(2) /= 5) error stop 2

    d%c = 1
    associate (p => (d(2)%c))
        d(2)%c = 5
        if (p /= 1) error stop 3
    end associate
end program
