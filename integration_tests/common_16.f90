! Test: COMMON block alignment edge case
! Tests storage association with types that might need alignment padding
! if the struct were not packed. COMMON blocks should be storage-associated
! (consecutive in memory) regardless of natural alignment requirements.
program common_16
    implicit none
    integer(1) :: i1
    real(8) :: r8
    integer(4) :: i4
    common /align/ i1, r8, i4

    i1 = 42
    r8 = 3.14159d0
    i4 = 999

    call sub_verify()
    if (i1 /= 100) error stop "i1 should be 100 after subroutine"
    print *, "PASS: common_16"
end program

subroutine sub_verify()
    implicit none
    ! Same layout - verify values persist
    integer(1) :: a
    real(8) :: b
    integer(4) :: c
    common /align/ a, b, c

    if (a /= 42) error stop "a should be 42"
    if (abs(b - 3.14159d0) > 1.0d-10) error stop "b should be pi"
    if (c /= 999) error stop "c should be 999"

    a = 100
end subroutine
