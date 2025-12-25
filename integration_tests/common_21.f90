! Test: COMMON block with mixed type sizes and type punning via storage association
! Verifies that the same storage can be accessed as different types (type aliasing)
! and that modifications through one view are visible through another view.
! This is valid Fortran behavior per F2018 8.10.3 (storage association).
program common_21
    implicit none
    integer(4) :: i4_1, i4_2
    real(8) :: r8
    common/mixblk/i4_1, i4_2, r8

    ! Initialize with known values
    i4_1 = 11
    i4_2 = 22
    r8 = 3.14159265358979d0

    ! Verify same layout can read values
    call sub_verify_layout()

    ! Modify storage through different type view (type punning)
    call sub_single_real8_view()

    ! Verify modification via array view
    call sub_array_view()

    ! Final verification: check that arr modifications are visible here
    ! After sub_array_view sets arr(1)=100, arr(2)=200, we should see:
    ! - r8 should be 200.0d0 (from arr(2))
    if (abs(r8 - 200.0d0) > 1.0d-10) error stop &
        "r8 should be 200.0 after array modification"

    print *, "PASS: common_21"
end program

subroutine sub_verify_layout()
    implicit none
    ! Same layout as main program - verify values are preserved
    integer(4) :: a, b
    real(8) :: c
    common/mixblk/a, b, c

    if (a /= 11) error stop "a should be 11"
    if (b /= 22) error stop "b should be 22"
    if (abs(c - 3.14159265358979d0) > 1.0d-10) error stop "c should be pi"
end subroutine

subroutine sub_single_real8_view()
    implicit none
    ! View entire 16 bytes as two real(8) values (type punning)
    ! First 8 bytes (i4_1 + i4_2) viewed as r8_first
    ! Second 8 bytes (r8) viewed as r8_second
    real(8) :: r8_first
    real(8) :: r8_second
    common/mixblk/r8_first, r8_second

    ! Modify through this view - changes underlying storage
    r8_first = 1.0d0
    r8_second = 2.0d0
end subroutine

subroutine sub_array_view()
    implicit none
    ! View entire 16 bytes as an array of 2 real(8) values
    real(8) :: arr(2)
    common/mixblk/arr

    ! Verify values set by sub_single_real8_view are visible here
    if (abs(arr(1) - 1.0d0) > 1.0d-10) error stop "arr(1) should be 1.0"
    if (abs(arr(2) - 2.0d0) > 1.0d-10) error stop "arr(2) should be 2.0"

    ! Modify via array access - will be visible in main program
    arr(1) = 100.0d0
    arr(2) = 200.0d0
end subroutine
