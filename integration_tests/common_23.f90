! Test: Blank (unnamed) COMMON block
! Verifies storage association with the special blank common
program common_23
    implicit none
    integer :: i, j
    real :: x
    common i, j, x  ! Blank common (no name)

    i = 42
    j = 84
    x = 3.14

    call sub_blank_access()
    if (i /= 100) error stop "i should be 100 after subroutine"
    print *, "PASS: common_23"
end program

subroutine sub_blank_access()
    implicit none
    integer :: a, b
    real :: c
    common a, b, c  ! Same blank common

    if (a /= 42) error stop "a should be 42"
    if (b /= 84) error stop "b should be 84"
    if (abs(c - 3.14) > 0.001) error stop "c should be 3.14"

    a = 100
end subroutine
