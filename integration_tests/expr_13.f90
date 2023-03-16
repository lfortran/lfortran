program flip_sign_dead_code
    implicit none
    integer :: number
    real :: x, eps = 1e-6
    integer :: a = 1, b = 2
    integer, parameter :: ap = 1, bp = 2

    real :: c = 1.0, d = 2.0
    real, parameter :: cp = 1.0, dp = 2.0

    number = 123
    x = 5.5

    if (modulo(number, 2) == 1 ) x = -x
    if (abs(x - (-5.5)) > eps) error stop

    number = 124
    x = 5.5
    if (modulo(number, 2) == 1 ) x = -x
    if (abs(x - (5.5)) > eps) error stop

    if( ap == bp ) then
        print *, "ap == bp"
    else
        print *, "ap /= bp"
    end if

    if( cp == dp ) then
        print *, "cp == dp"
    else
        print *, "cp /= dp"
    end if

    if( a == b ) then
        print *, "a == b"
        if( ap == bp ) then
            print *, "ap == bp"
        else
            print *, "ap /= bp"
        end if
    else if( ap == cp ) then
        print *, "ap == cp"
        if( cp == dp ) then
            print *, "cp == dp"
        else
            print *, "cp /= dp"
        end if
    else if( c == d ) then
        print *, "c == d"
    end if

end program
