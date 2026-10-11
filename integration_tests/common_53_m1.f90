! The module procedures of common_53_m1 and common_53_m2 declare
! /common_53_blk/ with different names. Neither module uses the other.
! The block appears only in procedures, so every unit that declares it saves
! it: its values must survive between calls.
module common_53_m1
    implicit none
contains
    subroutine set1()
        integer :: a, b
        common /common_53_blk/ a, b
        save /common_53_blk/
        a = 7
        b = 8
    end subroutine set1
end module common_53_m1
