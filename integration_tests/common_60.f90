! A double precision overlay of a COMMON integer array starts at byte 4, so it
! is not 8-byte aligned. It is used directly, passed by reference to an
! external procedure and passed to an assumed-shape dummy argument.
program common_60
    implicit none
    integer :: iarr(7)
    double precision :: d, darr(3)
    common /common_60_blk/ iarr
    equivalence (iarr(2), d), (iarr(2), darr(1))
    interface
        subroutine common_60_bump(x)
            double precision, intent(inout) :: x
        end subroutine common_60_bump
        subroutine common_60_bump_all(a)
            double precision, intent(inout) :: a(:)
        end subroutine common_60_bump_all
    end interface
    iarr = 0
    d = 2.5d0
    call common_60_bump(d)
    if (d /= 3.5d0) error stop
    darr(2) = 1.0d0
    darr(3) = 7.0d0
    call common_60_bump_all(darr)
    if (darr(1) /= 4.5d0 .or. darr(2) /= 2.0d0 .or. darr(3) /= 8.0d0) error stop
    if (iarr(1) /= 0) error stop
end program common_60

subroutine common_60_bump(x)
    implicit none
    double precision, intent(inout) :: x
    x = x + 1.0d0
end subroutine common_60_bump

subroutine common_60_bump_all(a)
    implicit none
    double precision, intent(inout) :: a(:)
    a = a + 1.0d0
end subroutine common_60_bump_all
