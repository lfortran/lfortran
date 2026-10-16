! The names of common_52_left at swapped positions: a covers n of
! common_52_left, and n(2) covers a(2) of common_52_left.
module common_52_swap
    implicit none
    integer(4) :: a, n(2)
    common /common_52_blk/ a, n
contains
    subroutine bump()
        a = a + 1
        n = 2 * n
    end subroutine bump
end module common_52_swap
