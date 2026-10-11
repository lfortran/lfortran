module common_53_m2
    implicit none
contains
    subroutine get2(v)
        integer, intent(out) :: v(2)
        integer :: i, j
        common /common_53_blk/ i, j
        save /common_53_blk/
        v = [i, j]
    end subroutine get2
end module common_53_m2
