! Test: COMMON block bidirectional value sharing (F2018 8.10.3)
! Verifies that values written in one program unit are visible in others,
! and that modifications in subroutines are visible back in the main program.
! Also tests that different variable names can access the same storage.
program common_20
    implicit none
    integer :: a, b, c
    real :: x, y
    common/valblk/a, b, c, x, y

    ! Initialize all values
    a = 10
    b = 20
    c = 30
    x = 1.5
    y = 2.5

    ! Verify values can be read in subroutine
    call sub_read_verify()

    ! Verify values modified in subroutine are visible here
    if (a /= 100) error stop "a should be 100 after sub_read_verify"
    if (b /= 200) error stop "b should be 200 after sub_read_verify"
    if (c /= 300) error stop "c should be 300 after sub_read_verify"

    ! Test with different variable names
    call sub_diff_names()
    if (a /= 111) error stop "a should be 111 after sub_diff_names"

    print *, "PASS: common_20"
end program

subroutine sub_read_verify()
    implicit none
    integer :: a, b, c
    real :: x, y
    common/valblk/a, b, c, x, y

    ! Verify we can read values set in main
    if (a /= 10) error stop "a should be 10"
    if (b /= 20) error stop "b should be 20"
    if (c /= 30) error stop "c should be 30"
    if (abs(x - 1.5) > 0.001) error stop "x should be 1.5"
    if (abs(y - 2.5) > 0.001) error stop "y should be 2.5"

    ! Modify values - should be visible in main
    a = 100
    b = 200
    c = 300
end subroutine

subroutine sub_diff_names()
    implicit none
    ! Different variable names but same storage
    integer :: p, q, r
    real :: s, t
    common/valblk/p, q, r, s, t

    ! p, q, r should have values 100, 200, 300 from previous subroutine
    if (p /= 100) error stop "p should be 100"
    if (q /= 200) error stop "q should be 200"
    if (r /= 300) error stop "r should be 300"

    ! Modify first variable
    p = 111
end subroutine
