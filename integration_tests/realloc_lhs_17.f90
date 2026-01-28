! Test automatic reallocation with --descriptor-index-64
! This triggers the bug where hardcoded i32 constants are multiplied with i64 values
program realloc_lhs_17
    implicit none
    real, allocatable :: a(:), b(:), c(:)
    integer :: i

    ! Allocate source arrays
    allocate(a(10), b(10))
    do i = 1, 10
        a(i) = real(i)
        b(i) = real(i * 2)
    end do

    ! c is NOT allocated - triggers automatic reallocation
    ! This should compute shape and allocate c to match a + b
    c = a + b

    ! Verify result - avoid using error stop with literal (ILP64 issue)
    if (size(c) /= 10) then
        print *, "FAIL: size(c) =", size(c)
        error stop
    end if
    if (abs(c(1) - 3.0) > 0.001) then
        print *, "FAIL: c(1) =", c(1)
        error stop
    end if
    if (abs(c(5) - 15.0) > 0.001) then
        print *, "FAIL: c(5) =", c(5)
        error stop
    end if
    if (abs(c(10) - 30.0) > 0.001) then
        print *, "FAIL: c(10) =", c(10)
        error stop
    end if

    print *, "PASS"
end program
