! A module declares a COMMON block, and the next program unit of the same
! file declares it again: it is associated with the block from its start.
module common_50_mod1
    implicit none
    integer :: a
    common /common_50_blk1/ a
end module common_50_mod1

subroutine common_50_same_name()
    implicit none
    integer :: a
    common /common_50_blk1/ a
    a = 7
end subroutine common_50_same_name

module common_50_mod2
    implicit none
    integer :: b
    common /common_50_blk2/ b
end module common_50_mod2

program common_50
    use common_50_mod1, only: a
    use common_50_mod2, only: b
    implicit none
    integer :: i
    common /common_50_blk2/ i
    call common_50_same_name()
    print *, a
    if (a /= 7) error stop
    b = 11
    print *, i
    if (i /= 11) error stop
end program common_50
