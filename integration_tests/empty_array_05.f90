program empty_array_05
    implicit none

    character(len=3) :: c(2), c0(0), r3
    character(len=:), allocatable :: d(:), r
    integer :: i

    c(1) = 'abc'
    c(2) = 'xyz'

    ! a non empty character array reduces to one of its elements
    if (maxval(c) /= 'xyz') error stop
    if (minval(c) /= 'abc') error stop

    ! MAXVAL of a zero sized character array is a string of char(0) and
    ! MINVAL of one is a string of char(255)
    r3 = maxval(c0)
    do i = 1, 3
        if (ichar(r3(i:i)) /= 0) error stop
    end do
    r3 = minval(c0)
    do i = 1, 3
        if (ichar(r3(i:i)) /= 255) error stop
    end do

    ! the same holds for a zero sized array constructor
    r3 = maxval([character(len=3) :: ])
    do i = 1, 3
        if (ichar(r3(i:i)) /= 0) error stop
    end do
    r3 = minval([character(len=3) :: ])
    do i = 1, 3
        if (ichar(r3(i:i)) /= 255) error stop
    end do

    ! and for a deferred length array allocated with size zero
    allocate(character(len=4) :: d(0))
    r = maxval(d)
    if (len(r) /= 4) error stop
    do i = 1, 4
        if (ichar(r(i:i)) /= 0) error stop
    end do
    r = minval(d)
    if (len(r) /= 4) error stop
    do i = 1, 4
        if (ichar(r(i:i)) /= 255) error stop
    end do
    deallocate(d)

    ! the elements of a deferred length array may have zero length themselves
    allocate(character(len=0) :: d(5))
    if (len(maxval(d)) /= 0) error stop
    if (len(minval(d)) /= 0) error stop
    deallocate(d)

    ! a deferred length array with elements of non zero length
    allocate(character(len=3) :: d(2))
    d(1) = 'abc'
    d(2) = 'xyz'
    if (maxval(d) /= 'xyz') error stop
    if (minval(d) /= 'abc') error stop
    deallocate(d)

    print *, "Pass"
end program
