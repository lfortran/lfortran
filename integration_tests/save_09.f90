program main
    implicit none
    call my_sub()
    call my_sub()
    contains

    subroutine my_sub()
        logical, save :: logicalFirstCall = .TRUE.
        integer, save :: intFirstCall = 1
        print *, logicalFirstCall
        print *, intFirstCall
        logicalFirstCall = .FALSE.
        intFirstCall = 10
    end subroutine

end program main
