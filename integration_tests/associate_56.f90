program associate_56
    implicit none
    integer :: a, b

    a = 1
    b = 2
    associate (tmp => a)
        a = b
        if (tmp /= 2) error stop 1
    end associate

    a = 1
    b = 2
    associate (tmp => (a))
        a = b
        b = tmp
    end associate

    if (a /= 2) error stop 2
    if (b /= 1) error stop 3
end program associate_56
