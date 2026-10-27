! submodule_62_r is used by the program and by two submodules of different
! modules; submodule_62_q only by those two submodules. Each module must
! have one state, whichever object file defines it.
module submodule_62_r
    implicit none
    integer :: r_total = 0
contains
    integer function r_bump()
        integer, save :: k = 0
        k = k + 1
        r_total = r_total + 10
        r_bump = k
    end function
end module

module submodule_62_q
    implicit none
    integer :: q_total = 0
contains
    integer function q_bump()
        integer, save :: k = 100
        k = k + 1
        q_total = q_total + 1
        q_bump = k
    end function
end module
