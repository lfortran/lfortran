! Test array section bounds with -fdefault-integer-8 (ILP64 mode)
! This tests that array bounds computation uses consistent integer types
! when the default integer kind is 64-bit.

program array_section_11
    implicit none
    integer, parameter :: n = 5, lwork = 10
    real :: work(lwork)
    integer :: i

    work = 0.0
    call caller(work, lwork, n)

    ! Check: elements 1-5 should be 0, elements 6-10 should be 1-5
    do i = 1, n
        if (work(i) /= 0.0) error stop
    end do
    do i = n+1, lwork
        if (work(i) /= real(i - n)) error stop
    end do

    print *, "PASSED"
end program
