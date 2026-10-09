! Two program units declare the same COMMON block with the same names at
! swapped positions: the objects are associated by storage position.
subroutine common_47_set()
    implicit none
    integer :: n, a
    common /common_47_blk/ n, a
    n = 3
    a = 4
end subroutine common_47_set

subroutine common_47_swapped()
    implicit none
    integer :: a, n
    common /common_47_blk/ a, n
    call common_47_set()
    print *, a, n
    if (a /= 3) error stop
    if (n /= 4) error stop
end subroutine common_47_swapped

program common_47
    implicit none
    call common_47_swapped()
end program common_47
