program cond_02
implicit none
    integer :: a = 1, b = 2
    integer, parameter :: ap = 1, bp = 2

    real :: c = 1.0, d = 2.0
    real, parameter :: cp = 1.0, dp = 2.0

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

end
