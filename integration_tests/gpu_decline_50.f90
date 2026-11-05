! gpu decline reason: ScalarNotNumeric, ArrayElementNotNumeric
! A deferred-length character variable used in a loop is a kernel argument
! that is not a number, as one of a constant length is, whether the loop
! reads a character of it, its length in the body or its length in a bound.
program gpu_decline_50
    implicit none
    integer :: i
    integer :: z(7)
    character(len=:), allocatable :: s
    character(len=:), allocatable :: a(:)
    character(len=:), pointer :: p
    s = "abcdefg"

    z = 0
    do concurrent (i = 1:7)
        if (s(i:i) == "c") z(i) = 1
    end do
    if (sum(z) /= 1) error stop "s(i:i)"
    if (z(3) /= 1) error stop "s(i:i) index"

    z = 0
    do concurrent (i = 1:7)
        z(i) = len(s)
    end do
    if (any(z /= 7)) error stop "len(s) in the body"

    z = 0
    do concurrent (i = 1:len(s))
        z(i) = i
    end do
    if (sum(z) /= 28) error stop "len(s) in a bound"

    allocate(character(len=3) :: a(7))
    do i = 1, 7
        a(i) = repeat(achar(iachar("a") + i - 1), i)
    end do
    z = 0
    do concurrent (i = 1:7)
        if (a(i)(1:1) == achar(iachar("a") + i - 1)) z(i) = len(a(i))
    end do
    if (any(z /= 3)) error stop "deferred-length array"

    allocate(character(len=5) :: p)
    p = "vwxyz"
    z = 0
    do concurrent (i = 1:5)
        if (p(i:i) == "x") z(i) = len(p)
    end do
    if (sum(z) /= 5 .or. z(3) /= 5) error stop "deferred-length pointer"
    deallocate(p)
    print *, "ok"
end program
