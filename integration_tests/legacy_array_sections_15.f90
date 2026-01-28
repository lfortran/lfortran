! Test legacy array sections with 64-bit default integers
! Tests that ArrayBound nodes use correct integer kind when -fdefault-integer-8 is used

subroutine caller(work, lwork, n)
    integer :: lwork, n
    real :: work(lwork)
    call callee(work(n+1), lwork-n)
end subroutine

subroutine callee(x, m)
    integer :: m
    real :: x(m)
    x(1) = 42.0
end subroutine

program legacy_array_sections_15
    implicit none
    real :: w(100)
    integer :: i
    w = 0.0
    call caller(w, 100, 10)
    if (abs(w(11) - 42.0) > 1e-6) error stop "Test failed: w(11) should be 42.0"
    print *, "PASS"
end program
