! Test: COMMON block with mixed type sizes and alignment considerations
! Tests various integer and real sizes in storage association
program common_21
    implicit none
    integer(4) :: i4_1, i4_2
    real(8) :: r8
    common /mixblk/ i4_1, i4_2, r8

    i4_1 = 11
    i4_2 = 22
    r8 = 3.14159265358979d0

    call sub_verify_layout()
    call sub_single_real8_view()
    call sub_array_view()
    print *, "PASS: common_21"
end program

subroutine sub_verify_layout()
    implicit none
    ! Same layout, verify values are preserved
    integer(4) :: a, b
    real(8) :: c
    common /mixblk/ a, b, c

    if (a /= 11) error stop "a should be 11"
    if (b /= 22) error stop "b should be 22"
    if (abs(c - 3.14159265358979d0) > 1.0d-10) error stop "c should be pi"
end subroutine

subroutine sub_single_real8_view()
    implicit none
    ! View first 8 bytes (i4_1 + i4_2) as a single real(8)
    ! This reinterprets two integers as one double
    real(8) :: r8_first
    real(8) :: r8_second
    common /mixblk/ r8_first, r8_second

    ! Just verify we can access - values are reinterpreted
    ! Modify and verify it affects the original storage
    r8_first = 1.0d0
    r8_second = 2.0d0
end subroutine

subroutine sub_array_view()
    implicit none
    ! View entire 16 bytes as an array of 2 real(8) values
    real(8) :: arr(2)
    common /mixblk/ arr

    ! Values should be what sub_single_real8_view set
    if (abs(arr(1) - 1.0d0) > 1.0d-10) error stop "arr(1) should be 1.0"
    if (abs(arr(2) - 2.0d0) > 1.0d-10) error stop "arr(2) should be 2.0"

    ! Modify via array access
    arr(1) = 100.0d0
    arr(2) = 200.0d0
end subroutine
