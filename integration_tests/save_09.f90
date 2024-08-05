program main
    implicit none
    call my_sub()
    call my_sub()
    call my_sub()
    contains

    subroutine my_sub()
        logical, save :: logicalFirstCall = .TRUE.
        integer, save :: intFirstCall = 1
        integer, save :: intArrayFirstCall(2) = [1, 2]
        character(len=2), save :: charFirstCall = "ab"
        print *, logicalFirstCall
        print *, intFirstCall
        print *, intArrayFirstCall
        print *, charFirstCall
        logicalFirstCall = .FALSE.
        intFirstCall = 10
        intArrayFirstCall = [3, 4]
    end subroutine

end program main
