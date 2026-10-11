! Declares the block of common_54_mod in a file of its own. The two files
! may be compiled in either order.
subroutine common_54_legacy(value)
    implicit none
    integer, intent(in) :: value
    integer :: b(3)
    common /common_54_blk/ b
    b = [value, value + 1, value + 2]
end subroutine common_54_legacy
