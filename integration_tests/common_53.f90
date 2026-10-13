! Procedures that use common_53_m1 declare /common_53_blk/ again, with the
! names of set1 at the same and at swapped positions: COMMON objects are
! associated by storage position, not by name.
subroutine common_53_same_names()
    use common_53_m1, only: set1
    implicit none
    integer :: a, b
    common /common_53_blk/ a, b
    save /common_53_blk/
    call set1()
    if (a /= 7) error stop
    if (b /= 8) error stop
    a = 9
end subroutine common_53_same_names

subroutine common_53_swapped_names()
    use common_53_m1, only: set1
    implicit none
    integer :: b, a
    common /common_53_blk/ b, a
    save /common_53_blk/
    if (b /= 9) error stop
    if (a /= 8) error stop
    call set1()
    if (b /= 7) error stop
end subroutine common_53_swapped_names

program common_53
    use common_53_m2, only: get2
    implicit none
    integer :: v(2)
    call common_53_same_names()
    call get2(v)
    print *, v
    if (any(v /= [9, 8])) error stop
    call common_53_swapped_names()
end program common_53
