program intrinsics_510
    ! Array intrinsics of character arrays whose length is not a constant:
    ! the length n of the elements of [word(1:n), 'yo'] and of a dummy, and
    ! an assumed length
    implicit none
    character(len=5) :: word
    character(len=:), allocatable :: r(:), r2(:,:), s
    integer :: n

    word = 'hi'
    n = 2

    r = pack([word(1:n), 'yo'], [.true., .false.])
    if (len(r) /= 2 .or. size(r) /= 1) error stop 1
    if (r(1) /= 'hi') error stop 2

    r = cshift([word(1:n), 'yo'], 1)
    if (len(r) /= 2 .or. size(r) /= 2) error stop 3
    if (r(1) /= 'yo' .or. r(2) /= 'hi') error stop 4

    r = eoshift([word(1:n), 'yo'], 1, 'zz')
    if (len(r) /= 2 .or. size(r) /= 2) error stop 5
    if (r(1) /= 'yo' .or. r(2) /= 'zz') error stop 6

    s = maxval([word(1:n), 'yo'])
    if (len(s) /= 2 .or. s /= 'yo') error stop 7
    s = minval([word(1:n), 'yo'])
    if (len(s) /= 2 .or. s /= 'hi') error stop 8

    r2 = transpose(reshape([word(1:n), 'yo'], [1, 2]))
    if (len(r2) /= 2 .or. size(r2, 1) /= 2 .or. size(r2, 2) /= 1) error stop 9
    if (r2(1, 1) /= 'hi' .or. r2(2, 1) /= 'yo') error stop 10

    call check(n, ['ab', 'cd'])
    call assumed_len(['xyz', 'abc'])

    print *, "OK"

contains

    ! maxval and minval of an assumed-length array, in the same file as those
    ! of arrays whose length is a run-time expression
    subroutine assumed_len(c)
        character(len=*), intent(in) :: c(:)
        character(len=:), allocatable :: t
        t = maxval(c)
        if (len(t) /= 3 .or. t /= 'xyz') error stop 14
        if (minval(c) /= 'abc') error stop 15
        if (len(maxval(c)) /= 3) error stop 16
    end subroutine assumed_len

    subroutine check(m, a)
        integer, intent(in) :: m
        character(len=m), intent(in) :: a(:)
        character(len=:), allocatable :: t(:)
        t = pack(a, [.false., .true.])
        if (len(t) /= 2 .or. size(t) /= 1) error stop 11
        if (t(1) /= 'cd') error stop 12
        if (maxval(a) /= 'cd') error stop 13
    end subroutine check

end program intrinsics_510
