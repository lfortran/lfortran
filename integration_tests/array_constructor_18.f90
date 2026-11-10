program array_constructor_18
    ! Without a type-spec, the elements of an array constructor have the
    ! length of its first element, here the length of trim(word) or of a
    ! concatenation with it
    implicit none
    integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
    character(len=5) :: word
    character(len=5), parameter :: pword = 'hi'
    character(len=3) :: parts(2)
    character(len=:), allocatable :: words(:)
    character(len=16) :: line
    character(len=10) :: base, sfx
    character(len=5) :: lines(2)
    character(kind=ucs4, len=6) :: w4
    character(len=:), allocatable :: d(:)
    character(len=4) :: p4(2)
    integer :: i, k

    word = 'hi'

    parts = [trim(word), 'yo'] // '!'
    if (parts(1) /= 'hi!') error stop 1
    if (parts(2) /= 'yo!') error stop 2

    if (len([trim(word), 'yo']) /= 2) error stop 3

    words = [trim(word), 'yo']
    if (len(words) /= 2) error stop 4
    if (size(words) /= 2) error stop 5
    if (words(1) /= 'hi' .or. words(2) /= 'yo') error stop 6

    write (line, '(a,"|",a)') [trim(word), 'yo']
    if (line /= 'hi|yo') error stop 7

    call check([trim(word), 'yo'])

    if (len([trim(pword), 'yo']) /= 2) error stop 8
    parts = [trim(pword), 'yo'] // '!'
    if (parts(1) /= 'hi!' .or. parts(2) /= 'yo!') error stop 9

    ! The first element is a concatenation with trim
    base = 'run'
    sfx = 'end'
    words = [trim(base)//'_x', trim(base)//'_y']
    if (len(words) /= 5 .or. size(words) /= 2) error stop 12
    if (words(1) /= 'run_x' .or. words(2) /= 'run_y') error stop 13

    if (len([trim(word)//'!', 'yo!']) /= 3) error stop 14
    parts = [trim(word)//'!', 'yo!']
    if (parts(1) /= 'hi!' .or. parts(2) /= 'yo!') error stop 15

    words = [trim(base)//'_'//trim(sfx), 'abc_def']
    if (len(words) /= 7) error stop 16
    if (words(1) /= 'run_end' .or. words(2) /= 'abc_def') error stop 17

    w4 = ucs4_'xyz'
    if (len([trim(w4)//ucs4_'!', ucs4_'abcd']) /= 4) error stop 18

    ! The constructor is passed to array intrinsics
    words = pack([trim(word), 'yo'], [.false., .true.])
    if (len(words) /= 2 .or. size(words) /= 1) error stop 19
    if (words(1) /= 'yo') error stop 20
    words = cshift([trim(word)//'!', 'yo!'], 1)
    if (len(words) /= 3) error stop 21
    if (words(1) /= 'yo!' .or. words(2) /= 'hi!') error stop 22
    if (maxval([trim(word)//'!', 'yo!']) /= 'yo!') error stop 23

    ! In an implied-do loop, the length of the elements may depend on the
    ! loop variable
    lines = ['ab   ', 'cd   ']
    words = [([trim(lines(i)), 'yo'], i = 1, 2)]
    if (size(words) /= 4) error stop 24
    if (trim(words(1)) /= 'ab' .or. trim(words(2)) /= 'yo') error stop 25
    if (trim(words(3)) /= 'cd' .or. trim(words(4)) /= 'yo') error stop 26

    ! A function called in the first element is called once
    d = ['ab ', 'cde']
    k = 0
    call check_d([d(nxt()), 'xyz'])
    if (k /= 1) error stop 27
    k = 0
    words = [d(nxt()), 'xyz']
    if (k /= 1) error stop 28
    if (len(words) /= 3) error stop 29
    if (words(1) /= 'ab' .or. words(2) /= 'xyz') error stop 30
    k = 0
    p4 = [d(nxt()), 'xyz']
    if (k /= 1) error stop 31
    if (p4(1) /= 'ab' .or. p4(2) /= 'xyz') error stop 32

    print *, "OK"

contains

    subroutine check(a)
        character(len=*), intent(in) :: a(:)
        if (len(a) /= 2) error stop 10
        if (a(1) /= 'hi' .or. a(2) /= 'yo') error stop 11
    end subroutine check

    subroutine check_d(a)
        character(len=*), intent(in) :: a(:)
        if (len(a) /= 3) error stop 33
        if (a(1) /= 'ab' .or. a(2) /= 'xyz') error stop 34
    end subroutine check_d

    integer function nxt()
        k = k + 1
        nxt = k
    end function nxt

end program array_constructor_18
