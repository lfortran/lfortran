! An array passed to a scalar dummy of a procedure defined outside any
! module in this file: with --legacy-array-sections its first element is
! passed, whether the procedure comes before or after the call.
subroutine legacy_array_sections_26_before(n)
    integer :: n
    if (n /= 5) error stop
    n = 7
end subroutine

subroutine legacy_array_sections_26_caller(iw)
    integer :: iw(*)
    call legacy_array_sections_26_before(iw)
    call legacy_array_sections_26_after(iw)
end subroutine

subroutine legacy_array_sections_26_after(n)
    integer :: n
    if (n /= 7) error stop
    n = 9
end subroutine

program legacy_array_sections_26
    implicit none
    integer :: iw(3)
    iw = [5, 6, 8]
    call legacy_array_sections_26_caller(iw)
    if (iw(1) /= 9) error stop
    if (iw(2) /= 6 .or. iw(3) /= 8) error stop
    print *, iw
end program
