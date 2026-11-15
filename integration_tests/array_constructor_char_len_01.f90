program array_constructor_char_len_01
    ! A type-spec gives every element of the constructor its length
    implicit none
    type :: array_constructor_char_len_01_holder
        character(len=8) :: names(2)
    end type
    type(array_constructor_char_len_01_holder) :: h
    character(len=6) :: word
    character(len=2) :: short
    character(len=8) :: cut(3)
    integer :: m, i

    word = 'planet'
    short = 'ab'
    m = 4

    cut = [character(len=3) :: word, 'x', short]
    if (cut(1) /= 'pla' .or. cut(2) /= 'x' .or. cut(3) /= 'ab') error stop 1

    cut = [character(len=m) :: word, word, short]
    if (cut(1) /= 'plan' .or. cut(2) /= 'plan' .or. cut(3) /= 'ab') error stop 2

    cut = [character(len=4) :: short, word, 'abcdefgh']
    if (cut(1) /= 'ab' .or. cut(2) /= 'plan' .or. cut(3) /= 'abcd') error stop 3
    if (cut(1)(3:8) /= '      ') error stop 4

    cut = [character(len=3) :: (word, i = 1, 3)]
    if (any(cut /= 'pla')) error stop 5

    cut = [character(len=3) :: [word, word], short]
    if (cut(1) /= 'pla' .or. cut(2) /= 'pla' .or. cut(3) /= 'ab') error stop 6

    cut = 'zzzzzzzz'
    cut(1:2) = [character(len=m) :: word, short]
    if (cut(1) /= 'plan' .or. cut(2) /= 'ab' .or. cut(3) /= 'zzzzzzzz') error stop 7

    h%names = [character(len=3) :: word, short]
    if (h%names(1) /= 'pla' .or. h%names(2) /= 'ab') error stop 8

    h%names = [character(len=m) :: word, short]
    if (h%names(1) /= 'plan' .or. h%names(2) /= 'ab') error stop 9

    cut = [character(len=8) :: word, short, 'x']
    if (cut(1) /= 'planet' .or. cut(2) /= 'ab' .or. cut(3) /= 'x') error stop 10

    cut = 'zz'
    where ([.true., .false., .true.]) cut = [character(len=3) :: word, short, word]
    if (cut(1) /= 'pla' .or. cut(2) /= 'zz' .or. cut(3) /= 'pla') error stop 11

    call fill_automatic()

    call fill_assumed(h%names, 3)
    if (h%names(1) /= 'pla' .or. h%names(2) /= 'xy') error stop 14
    print *, "OK"
contains
    subroutine fill_automatic()
        character(len=m) :: loc(2)
        loc = [character(len=3) :: word, short]
        if (loc(1) /= 'pla' .or. loc(2) /= 'ab') error stop 12
        loc = [character(len=m) :: word, short]
        if (loc(1) /= 'plan' .or. loc(2) /= 'ab') error stop 13
    end subroutine

    subroutine fill_assumed(a, n)
        character(len=*), intent(out) :: a(2)
        integer, intent(in) :: n
        a = [character(len=n) :: word, 'xy']
    end subroutine
end program
