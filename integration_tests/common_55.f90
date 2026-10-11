! Program units of one file declare named COMMON blocks with the same names
! at swapped positions, or after a module that declares the block: COMMON
! objects are associated by storage position, not by name.
module common_55_mod
    implicit none
    integer :: m
    common /common_55_mblk/ m
end module common_55_mod

subroutine common_55_after_module()
    implicit none
    integer :: i
    common /common_55_mblk/ i
    i = 7
end subroutine common_55_after_module

subroutine common_55_set()
    implicit none
    integer :: n, a
    common /common_55_blk/ n, a
    n = 3
    a = 4
end subroutine common_55_set

subroutine common_55_swapped()
    implicit none
    integer :: a, n
    common /common_55_blk/ a, n
    call common_55_set()
    print *, a, n
    if (a /= 3) error stop
    if (n /= 4) error stop
end subroutine common_55_swapped

program common_55
    use common_55_mod, only: m
    implicit none
    call common_55_swapped()
    call common_55_after_module()
    print *, m
    if (m /= 7) error stop
end program common_55
