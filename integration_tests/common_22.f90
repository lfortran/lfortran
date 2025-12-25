! Test: Multiple COMMON blocks in same program unit
! Verifies independent storage for different COMMON block names
program common_22
    implicit none
    integer :: a1, a2
    real :: b1, b2
    common /blk1/ a1, a2
    common /blk2/ b1, b2

    a1 = 10
    a2 = 20
    b1 = 1.5
    b2 = 2.5

    call sub_access_both()
    call sub_verify_independence()
    print *, "PASS: common_22"
end program

subroutine sub_access_both()
    implicit none
    integer :: x1, x2
    real :: y1, y2
    common /blk1/ x1, x2
    common /blk2/ y1, y2

    ! Verify both blocks accessible
    if (x1 /= 10) error stop "x1 should be 10"
    if (x2 /= 20) error stop "x2 should be 20"
    if (abs(y1 - 1.5) > 0.001) error stop "y1 should be 1.5"
    if (abs(y2 - 2.5) > 0.001) error stop "y2 should be 2.5"

    ! Modify both blocks
    x1 = 100
    y1 = 10.5
end subroutine

subroutine sub_verify_independence()
    implicit none
    ! Access only blk1 - should not affect blk2
    integer :: p, q
    common /blk1/ p, q

    if (p /= 100) error stop "p should be 100"
    p = 999
end subroutine
