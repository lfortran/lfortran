!   Test combination of implicit-interface and intrinsic functions
!   Check if intrinsic function is used when it is declared as an integer
    subroutine fppasu()
    integer max0
    a = max0(1,2)
    b = max0(1,2/2,1)
    end
