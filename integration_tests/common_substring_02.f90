! Reproducer: COMMON block CHARACTER substring using the same variable for
! both bounds, e.g. ISPARS(K:K), collapses to a StringItem which must still
! point to the COMMON struct member (reduced from XFOIL src/xfoil.f).
program common_substring_02
    implicit none
    character(len=80) :: ispars
    integer :: k
    common /cc01/ ispars

    ispars = "ABCDE"
    k = 3

    ! ispars(k:k) with identical bounds becomes a single-character StringItem
    if (ispars(k:k) /= "C") error stop "single-char substring mismatch"

    k = 1
    if (ispars(k:k) /= "A") error stop "single-char substring mismatch 2"

    print *, "PASS: common_substring_02"
end program
